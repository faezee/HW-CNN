Library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use work.my_pack.all;

entity Line_buffer is
	--generic( --g_buff_depth : integer := 8; g_buff_width : integer := 16; g_index_length : integer := 6 --);
	port(
		-- read interface
		i_rd_enable : in std_logic;
		i_rd_index : in std_logic_vector(c_LB_index_length-1 downto 0);
		o_rd_data : out std_logic_vector(c_data_width-1 downto 0);

		-- write interface
		i_wr_data_valid : in std_logic;
		i_wr_data : in std_logic_vector(c_data_width-1 downto 0);
		i_wr_index : in std_logic_vector(c_LB_index_length-1 downto 0);
		
		-- flags
		clk : in std_logic;
		rst : in std_logic
	);
end Line_buffer;

architecture Behavioral of Line_buffer is

	type t_FIFO is array (0 to c_Line_Buffer_depth-1) of std_logic_vector(c_data_width-1 downto 0);
	signal r_data : t_FIFO;

	subtype index_type is integer range 0 to c_Line_Buffer_depth-1 ; 

	procedure incr(signal index: inout index_type) is
	begin
		if(index=c_Line_Buffer_depth-1) then
			index<= 0;
		else
			index<= index+1;
		end if;
	end procedure;
	
begin

	write_interface : process(clk)
	begin
	if(rising_edge(clk)) then
		if(rst='1') then
			r_data <= (others=>(others=>'0'));
		elsif(i_wr_data_valid='1') then
			r_data(to_integer(unsigned(i_wr_index))) <= i_wr_data;
			--incr(r_wr_index);
		end if;
	end if;
	end process;

	read_interface : process(clk)
	begin
	if(rising_edge(clk)) then
		if(i_rd_enable='1') then
			o_rd_data <= r_data(to_integer(unsigned(i_rd_index)));
		end if;
	end if;
	end process;

	--o_rd_data <= r_data(to_integer(unsigned(i_rd_index)));

end Behavioral;
