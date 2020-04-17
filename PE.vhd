library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use work.my_pack.all;

entity PE is
	generic(
		g_PE_number : integer := 0
	);
	port(
		--	from LB
		i_input_data : std_logic_vector(c_data_width-1 downto 0);
		
		--	from Row_controller
		----	filter loading
		i_filter_valid : in std_logic;
		i_filter_data : std_logic_vector(c_data_width-1 downto 0);
		----	input loading
		i_input_valid : in std_logic;
		
		o_mult_done : out std_logic;
		i_buff_rd_index : in std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
		clk : in std_logic;
		rst : in std_logic
	);
end PE;

architecture Behavioral of PE is
	
	component PE_Line_buffer is
	port(
		i_rd_index : in std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
		i_rd_enable : in std_logic;
		o_rd_data : out std_logic_vector(c_data_width-1 downto 0);
		i_wr_data_valid : in std_logic;
		i_wr_data : in std_logic_vector(c_data_width-1 downto 0);
		clk : in std_logic;
		rst : in std_logic
	);
	end component;

	component PE_Mult is
	generic(
		g_PE_number : integer := 0
	);
	port(
		i_new_input : in std_logic;
		i_input_feature : in std_logic_vector( c_data_width-1 downto 0);
		i_filter 	: in std_logic_vector( c_data_width-1 downto 0);
		o_done : out std_logic;
		o_mult_out : out std_logic_vector( c_data_width-1 downto 0);
		
		clk : in std_logic;
		rst : in std_logic
	);
	end component PE_Mult;

	signal  w_temp_filter : std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
	signal  r_mult_res : std_logic_vector(c_data_width-1 downto 0);

	signal  r_rd_index : integer range 0 to c_PE_buff_depth-1;
	signal  w_buff_rd_data : std_logic_vector(c_data_width-1 downto 0);

begin

	index_of_LB: process (clk)
	begin
		if rising_edge(clk) then
			if(i_input_valid='1') then
				if(r_rd_index = c_PE_buff_depth-1) then 
					r_rd_index <= 0;
				else
					r_rd_index <= r_rd_index +1;
				end if;
			end if;
		end if;
	end process;
	--w_temp_filter <= std_logic_vector(to_unsigned(r_rd_index, w_temp_filter'length));


	P_LB_ins : PE_Line_buffer
	port map(
		i_rd_index => i_buff_rd_index,
		i_rd_enable => i_input_valid,
		o_rd_data => w_buff_rd_data,
		i_wr_data_valid => i_filter_valid,
		i_wr_data => i_filter_data,
		clk => clk,
		rst => rst
	);

	Mult_ins : PE_Mult
	generic map(
		g_PE_number => g_PE_number
	)
	port map(
		i_new_input  => i_input_valid,
		i_input_feature => i_input_data,
		i_filter => w_buff_rd_data,
		o_done => o_mult_done,
		o_mult_out => r_mult_res,
		
		clk => clk,
		rst => rst
	);

	

end Behavioral;
