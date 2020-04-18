library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.my_pack.all;

entity Row_Controller is
	generic(
		g_addr_width : integer := 16;
		g_load_first : integer := 34;	-- (32+32+3)-1
		g_load_filter : integer := 8;	-- filter size 3*3 -1
		g_buff_depth : integer := 64;
		g_index_length : integer := 6;
		g_filter_dim_1 : integer := 3;
		g_filter_dim_2 : integer := 3;
		g_fet_dim_1 : integer := 16;
		g_mult_clk	: integer := 5	--	it takes g_mult_clk clocks to do the multiplying - (-1)
	);
	port(
		--	to line buffer
		o_LB_data_valid : out std_logic;	--  tell the LB that the data is valid
		o_LB_wr_index   : out std_logic_vector(c_LB_index_length-1 downto 0);
		o_LB_rd_enable	: out std_logic;	--	tell the LB that it is about to be readen :D 
		o_LB_rd_index	: out std_logic_vector(c_LB_index_length-1 downto 0);	--	the index that LB should be readen, this index is with o_LB_rd_enable
		
		--	interface to higher memory level
		----	for input feature loading
		o_start_addr : out std_logic_vector(c_addr_width-1 downto 0);
		o_GB_burst_addr_valid : inout std_logic;							-- is inout okay? :D
		o_GB_addr_valid : inout std_logic;										
		o_GB_end_burst : out std_logic;
		i_GB_data_ready : in std_logic;
		----	for filter loading

		--	processing elements (PE)
		o_PE_filter_valid : out std_logic_vector(c_PE_row_dim-1 downto 0);		--	tell the PE to store the buffer, the data comes from GB or DRAM
		o_PE_input_valid : out std_logic;
		i_mult_done : in std_logic;				--	consider again - if it's better that PE tells RC the mult is done.
		o_PE_LB_rd_index : out std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
	
		-- multiple_row_controller
		o_load_filter_done  : out std_logic;
		o_MRC_in_ld_frst_pass  : out std_logic;
		i_MRC_GB_available : in std_logic;
		
		--
		clk : in std_logic;
		rst : in std_logic;
		
		i_start_ld_input : in std_logic;
		i_start_ld_filter : in std_logic
	);
end Row_Controller;

architecture Behavioral of Row_Controller is

	type STATE_TYPE is (IDLE, REQ_INPUT_MEM, LD_BUFF, REQ_FILTER_MEM, LD_FILTER, BUFF_LOAD_SEND, BUFF_WAIT_MULT);
	signal state : STATE_TYPE;

	type READ_STATE_TYPE is (IDLE, BURST_READ, ONE_READ);
	signal read_state : READ_STATE_TYPE;

	type  FILTER_MOVE_TYPE is (SAME_ROW, LAST_ROW_EL , LAST_ROW, LAST_FILTER_EL, NEXT_ROW_FILTER, DONE);
	signal lb_read_state : FILTER_MOVE_TYPE;


	signal w_start_addr : std_logic_vector(g_addr_width-1 downto 0);		--	send the start address to higher level memory.
	signal r_count_ld_inputF : integer range 0 to c_size_load_inputF-1;		--	count the number of loaded pixels to line buffer
	
	signal r_count_ld_filter : integer range 0 to c_size_load_filter-1;

	--	signals to handle "loading data to LB" and "sending from LB to PE"
	
	signal  r_cnt_sent_elm_row : integer range 0 to g_filter_dim_1-1;			--	cnt elements of sent from one row 
	signal  r_cnt_sent_row : integer range 0 to c_filter_dim_2-1;				--	cnt number of sent rows
	signal  r_begin_index : integer range 0 to c_Line_Buffer_depth;				--	where each filter begins
	signal  r_begin_index_cnt_row  : integer range 0 to c_feature_dim_1;		--  this should be exactly (c_feature_dim_1-c_filter_dim_1)
	signal  r_cnt_filter_movement : integer range 0 to c_feature_dim_2;			--  to calculate when one filter is convolved in one input channel.
	signal  r_LB_rd_index : integer range 0 to c_Line_Buffer_depth-1 := 0;
	signal  r_cnt_row_filter_move : integer range 0 to c_feature_dim_2;			--  to calculate when one filter is convolved in one input channel.
	signal  r_conv_done : std_logic := '1';

	signal  r_LB_wr_index  : integer range 0 to c_Line_Buffer_depth-1;
	signal  r_LB_wr_data_val  : std_logic := '0';	
	signal  r_GB_end_burst : std_logic := '0' ;
	signal  r_PE_LB_rd_index  : integer range 0 to c_PE_buff_depth-1;
	signal  r_in_ld_frst_pass : std_logic;
	signal  r_cnt_mult : integer range 0 to g_mult_clk-1;
	signal  r_LB_fill_cnt : integer range 0 to c_Line_Buffer_depth-1;


	--	more than one PE - multiple PEs in one row
	signal  r_cnt_load_filter_PE : integer range 0 to c_PE_row_dim := 0;
	type  twoD_array  is array (0 to 3) of std_logic_vector(c_addr_width-1 downto 0);
	signal  r_addr_of_filter_in_GB : twoD_array := (x"0000", x"000A" ,x"0014" ,x"001E");
	signal  r_addr_feature_last_read : integer range 0 to c_GB_depth-1;
	signal  w_PE_filter_valid : std_logic;
	signal  r_load_filter_done : std_logic;

	procedure Incr_LB_index(signal index: inout integer range 0 to c_Line_Buffer_depth-1) is
	begin
		if(index = c_Line_Buffer_depth-1) then
			index <= 0;
		else
			index <= index+1;
		end if;
	end procedure;

	procedure Idx_go_next_row(signal index: inout integer range 0 to c_Line_Buffer_depth-1) is 
	begin
--		if(index = c_Line_Buffer_depth-1) then
--			index <= c_feature_dim_1 - g_filter_dim_1;
--		else 
--			index <= index + c_feature_dim_1 - g_filter_dim_1 +1;
--		end if;
		
		if(index >= c_Line_Buffer_depth - c_feature_dim_1 + g_filter_dim_1 -1) then
			index <= c_feature_dim_1 - g_filter_dim_1 +1 - c_Line_Buffer_depth + index;
		else
			index <= index + c_feature_dim_1 - g_filter_dim_1 +1;
		end if;
		
	end procedure;

	procedure Incr_PE_LB_index(signal index: inout integer range 0 to c_PE_buff_depth-1) is
	begin
		if(index = c_PE_buff_depth-1) then
			index <= 0;
		else
			index <= index+1;
		end if;
	end procedure;

	procedure Incr_GB_Addr(signal r_addr_feature_last_read : inout integer range 0 to c_GB_depth-1) is
	begin
		if(r_addr_feature_last_read = c_GB_depth-1) then 
			r_addr_feature_last_read <= 0;
		else 
			r_addr_feature_last_read <= r_addr_feature_last_read+ 1;
		end if;
	end procedure;

	

begin

	state_tr : process(clk)
	begin
	if(rising_edge(clk)) then
		if(rst='1') then
			state <= IDLE;
			--  o_PE_input_valid <= '0';
			r_PE_LB_rd_index <= 0;
			r_cnt_load_filter_PE <= 0;
			o_PE_filter_valid <= (others=>'0');
			r_cnt_filter_movement <= 0;
			r_count_ld_filter <= 0;
			r_in_ld_frst_pass <= '0';
		else
			r_load_filter_done <= '0';
			case state is
			when IDLE =>
				if(i_start_ld_filter ='1') then
					state <= REQ_FILTER_MEM;
				elsif (i_start_ld_input = '1') then		--	TODO_ the priority
					state <= REQ_INPUT_MEM;
				else
					state <= IDLE;
				end if;
			when REQ_FILTER_MEM => 
				if(i_GB_data_ready='1' and r_GB_end_burst='0') then		--	r_GB_end_burst condition is because of more than PE in one row
					state <= LD_FILTER;
					o_PE_filter_valid(r_cnt_load_filter_PE) <= '1';
				else 
					state <= REQ_FILTER_MEM;
				end if;
			when LD_FILTER =>
				if(r_count_ld_filter=c_size_load_filter-1 and r_cnt_load_filter_PE = c_PE_row_dim -1 ) then
					r_load_filter_done <= '1';
					state <= IDLE;
					r_count_ld_filter <= 0;
					r_cnt_load_filter_PE <= 0;
					o_PE_filter_valid(r_cnt_load_filter_PE) <= '0';
				elsif (r_count_ld_filter=c_size_load_filter-1 and r_cnt_load_filter_PE < c_PE_row_dim -1 ) then
					state <= REQ_FILTER_MEM;
					o_PE_filter_valid(r_cnt_load_filter_PE) <= '0';
					r_count_ld_filter <= 0;
					r_cnt_load_filter_PE <= r_cnt_load_filter_PE+ 1;
				else
					r_count_ld_filter <= r_count_ld_filter+1;
					state <= LD_FILTER;
				end if;
			when REQ_INPUT_MEM =>
				if(i_GB_data_ready='1') then	--	if data is available from higher memory.
					state <= LD_BUFF;
				else 
					state <= REQ_INPUT_MEM;
				end if;
			when LD_BUFF =>	
				if(r_count_ld_inputF=c_size_load_inputF-1) then
					state <= BUFF_LOAD_SEND;
					r_in_ld_frst_pass <= '1';
				else
					r_count_ld_inputF <= r_count_ld_inputF+1;
					state <= LD_BUFF;
				end if;
			when BUFF_LOAD_SEND	=>					--	load the data to LB & send the loaded data from LB to PE
				r_in_ld_frst_pass <= '0';
				state <= BUFF_WAIT_MULT;
				--o_PE_input_valid <= '1';
				Incr_PE_LB_index(r_PE_LB_rd_index);
			when BUFF_WAIT_MULT =>
				if(r_cnt_mult = g_mult_clk-1) then
					r_cnt_mult <= 0;
					state <= BUFF_LOAD_SEND;
				else 
					r_cnt_mult <= r_cnt_mult+1;
					state <= BUFF_WAIT_MULT;
				end if;
				--o_PE_input_valid <= '0';		--	TODO
			end case;
		end if;
	end if;
	end process;


	--	handle sending data from Line_buffer to PEs in one row .  the indexes of line_buffer for convolution
	rd_index_val_to_LB : process(clk)
	begin
	if(rising_edge(clk)) then
		if(rst='1') then
			r_conv_done <= '0';
			r_cnt_sent_row <= 0;
			r_cnt_sent_elm_row <= 0;
			r_begin_index <= 0;
			r_LB_rd_index <= 0;
		else
			case state is
			when BUFF_LOAD_SEND => 
				r_conv_done <= '0';
				case  lb_read_state is
				when  SAME_ROW =>		-- just plus one 
					Incr_LB_index(r_LB_rd_index);
					--
					r_cnt_sent_elm_row <= r_cnt_sent_elm_row +1;
					if(r_cnt_sent_elm_row = c_filter_dim_1-2) then
						lb_read_state <= LAST_ROW_EL;
					else
						lb_read_state <= SAME_ROW;
					end if;
				when  LAST_ROW_EL =>	--	last element of one row
					Idx_go_next_row(r_LB_rd_index);
					--
					r_cnt_sent_elm_row <= 0;
					r_cnt_sent_row <= r_cnt_sent_row+1;
					if(r_cnt_sent_row = c_filter_dim_2-2) then
						lb_read_state <= LAST_ROW;
					else
						lb_read_state <= SAME_ROW;
					end if;
				when  LAST_ROW =>		--	last element of last row of filter
					Incr_LB_index(r_LB_rd_index);
					--
					r_cnt_sent_elm_row <= r_cnt_sent_elm_row +1;
					if(r_cnt_sent_elm_row = c_filter_dim_1-2 and r_begin_index_cnt_row= c_feature_dim_1-c_filter_dim_1) then
						lb_read_state <= NEXT_ROW_FILTER;
					elsif(r_cnt_sent_elm_row = c_filter_dim_1-2) then
						lb_read_state <= LAST_FILTER_EL;
					else
						lb_read_state <= LAST_ROW;
					end if;
				when  LAST_FILTER_EL =>		--	last element of the filter - move filter - means change the r_begin_index
					if(r_begin_index = c_Line_Buffer_depth-1) then
						r_begin_index <= 0;
						r_LB_rd_index <= 0;
					else
						r_begin_index <= r_begin_index +1;
						r_LB_rd_index <= r_begin_index +1;
					end if;
					--
					r_cnt_sent_elm_row <=0;
					r_cnt_sent_row <= 0;
					r_begin_index_cnt_row <= r_begin_index_cnt_row+1;
					-- 
					lb_read_state <= SAME_ROW;
				when  NEXT_ROW_FILTER =>
					if(r_begin_index >= c_Line_Buffer_depth-c_filter_dim_1) then
						r_begin_index <= c_filter_dim_1- c_Line_Buffer_depth + r_begin_index;
						r_LB_rd_index <= c_filter_dim_1- c_Line_Buffer_depth + r_begin_index;
					else
						r_begin_index <= r_begin_index+ c_filter_dim_1;
						r_LB_rd_index <= r_begin_index+ c_filter_dim_1;
					end if;
					------------
					r_cnt_sent_elm_row <=0;
					r_cnt_sent_row <= 0;
					r_begin_index_cnt_row <= 0;
					r_cnt_row_filter_move <= r_cnt_row_filter_move+1;
					if(r_cnt_row_filter_move = c_feature_dim_2- c_filter_dim_2) then
						r_cnt_row_filter_move <= 0;
						lb_read_state <= DONE;
						r_conv_done <= '1';
					else
						lb_read_state <= SAME_ROW;
					end if;
				when  DONE =>
					r_conv_done <= '0';
				end case;	--  lb_read_state
			when others =>
				
			end case; 	--  state 
		end if;
	end if;
	end process;


	--	to line buffer, write index and write valid signals 
	--  to GB , tell to stop the burst
	wr_index_val_to_LB : process(clk) 
	begin
	if(rising_edge(clk)) then
		if(rst='1') then
			r_LB_wr_index <= 0;
			r_LB_wr_data_val <= '0';
			o_GB_addr_valid <= '0';
			r_addr_feature_last_read <= 64;
		else
			case state is
			when LD_FILTER =>
				if(r_count_ld_filter=c_size_load_filter-1) then		
					r_GB_end_burst <= '1';		--	'1' in just one clock cycle
				else
					r_GB_end_burst <= '0';							-- stay in LD_FILTER
				end if;
			when REQ_INPUT_MEM =>
				if(i_GB_data_ready='1') then
					r_LB_wr_data_val <= '1';
				end if;
			when LD_BUFF =>
				if(r_count_ld_inputF=c_size_load_inputF-1) then
					--read_state <= BURST_READ;			--change the next state to IDLE
					read_state <= IDLE;
					r_LB_wr_data_val <= '0';
					r_GB_end_burst <= '1';
					----------------------------
					Incr_LB_index(r_LB_wr_index);				--	this one needs a check, if the size of GB is exactlly c_size_load_inputF
					Incr_GB_Addr(r_addr_feature_last_read);
				else
					r_LB_wr_data_val <= '1';
					Incr_GB_Addr(r_addr_feature_last_read);
					Incr_LB_index(r_LB_wr_index);
				end if;
			when BUFF_LOAD_SEND | BUFF_WAIT_MULT =>
				--if(i_MRC_GB_available = '0') then
					--read_state <= IDLE;
				--else 
					case read_state is 
					when  IDLE =>
						r_LB_wr_data_val <= '0';
						r_GB_end_burst <= '0';
						if (r_LB_wr_index < r_begin_index-1 or (r_begin_index /= 0 and r_LB_wr_index=c_Line_Buffer_depth-1)) then
							--  an empty LB element is available now!
							read_state <= ONE_READ; 	--  TODO_imp , in what senario it will move to BURST_READ state
							o_GB_addr_valid <= '1';		--  tell the GB to send new data.
							Incr_GB_Addr(r_addr_feature_last_read);
						end if;
					when  BURST_READ =>
						if(r_LB_wr_index = r_begin_index-1 or (r_begin_index=0 and r_LB_wr_index=c_Line_Buffer_depth-1)) then
							--	no empty LB 
							read_state <= IDLE;
							r_LB_wr_data_val <= '0';
							r_GB_end_burst <= '1';
						else
							read_state <= BURST_READ;
							r_LB_wr_data_val <= '1';
							Incr_LB_index(r_LB_wr_index);
							Incr_GB_Addr(r_addr_feature_last_read);
						end if;
					when  ONE_READ =>
						if(i_GB_data_ready='1') then
							r_LB_wr_data_val <= '1';
							Incr_LB_index(r_LB_wr_index);
							read_state <= IDLE;
							o_GB_addr_valid <= '0';		--  tell the GB to send new data.
						else
							read_state <= ONE_READ;
						end if;
					end case;
				--end if;	--	end if(i_GB_available = '0')
			when others => 
				r_LB_wr_data_val <= '0';
				r_GB_end_burst <= '0';
			end case;
		end if;
	end if;
	end process;


	LB_fill_up : process(clk)	-- the sensitivity list can be the r_LB_wr_index and r_begin_index , but for now clk is enough
	begin
	if(rising_edge(clk)) then
		if(r_begin_index < r_LB_wr_index) then
			r_LB_fill_cnt <= c_Line_Buffer_depth - r_LB_wr_index + r_begin_index;
		else 
			r_LB_fill_cnt <= r_begin_index - r_LB_wr_index ;
		end if;
	end if;
	end process;


	-- OUTPUTS to higher memory level - GB
	----
	ld_filter_PEs : process(state, read_state)
	begin
		case state is
		when REQ_FILTER_MEM =>
			o_GB_burst_addr_valid <= '1';
			o_start_addr <= r_addr_of_filter_in_GB(r_cnt_load_filter_PE);
		when REQ_INPUT_MEM =>
			o_GB_burst_addr_valid <= '1';
			--o_start_addr <= (x"0040");
			o_start_addr <=  std_logic_vector(to_unsigned(r_addr_feature_last_read, o_start_addr'length)) ;
		when BUFF_LOAD_SEND | BUFF_WAIT_MULT =>
			case(read_state) is
			when ONE_READ => 
				o_start_addr <= std_logic_vector(to_unsigned(r_addr_feature_last_read, o_start_addr'length));
			when others =>
				o_start_addr <= std_logic_vector(to_unsigned(r_addr_feature_last_read, o_start_addr'length));  -- TODO
			end case;
		when others =>
			o_GB_burst_addr_valid <= '0';
		end case;
	end process;
	--	
	o_GB_end_burst <= r_GB_end_burst;


	--	OUTPUTS to line buffer
	o_LB_data_valid <= r_LB_wr_data_val;				--	tell the LB "data from GB is ready to store".
	o_LB_wr_index <= std_logic_vector(to_unsigned(r_LB_wr_index, o_LB_wr_index'length));
	o_LB_rd_index <= std_logic_vector(to_unsigned(r_LB_rd_index, o_LB_rd_index'length));
	o_LB_rd_enable <= '1' when state=BUFF_LOAD_SEND else '0';
	--o_LB_rd_enable <= '1' when state=BUFF_LOAD_SEND and r_LB_fill_cnt>=c_size_load_filter else '0';


	--	OUTPUTS to PE
	w_PE_filter_valid <= '1' when state=LD_FILTER else '0';			--	tell the PE that data is coming from GB
	o_PE_input_valid <= '1' when state=BUFF_LOAD_SEND else '0';
	o_PE_LB_rd_index <= std_logic_vector(to_unsigned(r_PE_LB_rd_index, o_PE_LB_rd_index'length));

	
	--  OUTPUTS to Multiple_Row_Controller
	o_load_filter_done <= r_load_filter_done;
	o_MRC_in_ld_frst_pass <= r_in_ld_frst_pass;


end Behavioral;