library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use STD.textio.all;
use work.my_pack.all;

entity Global_Buffer is
	port(

		--	interface to Row_Controller
		i_start_addr : in std_logic_vector(c_addr_width-1 downto 0);
		i_addr_valid : in std_logic;
		i_addr_burst_valid : in std_logic;
		o_data_ready : out std_logic;

		o_data : out std_logic_vector(c_data_width-1 downto 0);
		i_end_burst : in std_logic;


		clk : in std_logic;
		rst : in std_logic
	);
end Global_Buffer;

architecture Behavioral of Global_Buffer is 

	type t_MEM is array (0 to c_GB_depth-1) of std_logic_vector(c_data_width-1 downto 0);
	signal r_mem : t_MEM;

	type STATE_TYPE is (IDLE, BURST_READ, ONE_READ);
	signal state : STATE_TYPE;
	
	--signal  r_addr : std_logic_vector(c_addr_width-1 downto 0);
	signal  r_addr : integer range 0 to c_GB_depth-1;
	--signal  r_end_burst : std_logic := '0';
	
	--	read from file
	file file_VECTORS : text;
	
	procedure Next_Burst_Addr(signal r_addr : inout integer range 0 to c_GB_depth-1) is
	begin
		if(r_addr = c_GB_depth-1) then
			r_addr <= 0;
		else
			r_addr <= r_addr+1;
		end if;
	end procedure;

begin

	read_interface : process(clk) is 	--	TODO _ turn it to a suitable burst read interface
	begin
	if(rising_edge(clk)) then
		if(rst='1') then
			state <= IDLE;
			--r_end_burst <= '0';
		else
			case state is
			when IDLE =>
				if(i_addr_burst_valid='1') then
					state <= BURST_READ;
					r_addr <= to_integer(unsigned(i_start_addr));		-- register the address to do the burst.
				elsif (i_addr_valid='1') then
					state <= ONE_READ;
					r_addr <= to_integer(unsigned(i_start_addr));
				end if;
			when ONE_READ =>
				o_data <= r_mem(r_addr);
				state <= IDLE;
			when BURST_READ =>
				if(i_end_burst='1') then
					state <= IDLE;
					--r_end_burst <= '0';
				else
					o_data <= r_mem(r_addr);
					Next_Burst_Addr(r_addr);
					--r_end_burst <= i_end_burst;		--	r_end_burst is meaningful whenever we are in BURST_READ state,
				end if;
			end case;
		end if;
	end if;
	end process;

	--  IMP _ data will be ready in next clock cycle, in that ONE clock cyle, the RC will tell the PE the data will be ready...
	o_data_ready <= '1' when (state=BURST_READ or state=ONE_READ) else '0';			

	process 
		variable v_ILINE     : line;
		variable counter : integer := 0;
		variable v_term : std_logic_vector(c_data_width-1 downto 0);
	begin
		file_open(file_VECTORS, "input_vectors.txt",  read_mode);
		while not endfile(file_VECTORS) loop
			readline(file_VECTORS, v_ILINE);
			read(v_ILINE, v_term);
			r_mem(counter) <= v_term;
			counter:= counter+1;
		end loop;
		file_close(file_VECTORS);
		wait;
	end process;


end Behavioral;
