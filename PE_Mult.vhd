library IEEE;
use IEEE.STD_LOGIC_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use STD.textio.all;
use work.my_pack.all;

entity PE_Mult is
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
end entity PE_Mult;

architecture Behavioral of PE_Mult is 

	type STATE_TYPE is (IDLE, MULTIPLYING);
	signal state : STATE_TYPE;
	signal r_cnt_mult : integer range 0 to c_mult_clk-1;
	signal r_mult_res : std_logic_vector(15 downto 0);
	signal r_mult_res_log : std_logic_vector(15 downto 0);

	file file_RESULTS : text;
	signal int_alaki : integer range 0 to 3;
	signal file_name: string(1 to 13) := "mult_result_"&INTEGER'IMAGE(g_PE_number);
begin

	file_open(file_RESULTS, file_name,  write_mode);
	process(clk)
		variable v_OLINE : line;
	begin
		
		if rising_edge(clk) then
			if(rst='1') then
				--file_open(file_RESULTS, "mult_res.txt",  write_mode);
				r_cnt_mult <= 0;
				o_done <= '0';
			else				
				case state is
				when IDLE =>
					if(i_new_input='1') then
						state <= MULTIPLYING;
						o_done <= '0';
					end if;
				when MULTIPLYING => 
					if(r_cnt_mult < c_mult_clk-1) then
						state <= MULTIPLYING;
						r_cnt_mult <= r_cnt_mult+1;
						r_mult_res_log <= std_logic_vector(unsigned(i_input_feature)*unsigned(i_filter));
					else
						r_cnt_mult <= 0;
						state <= IDLE;
						o_done <= '1';
						write(v_OLINE, r_mult_res_log);
						writeline(file_RESULTS, v_OLINE);
					end if;
				end case;
			end if;
		end if;
	end process;
	
	r_mult_res <= std_logic_vector(unsigned(i_input_feature)*unsigned(i_filter));
	o_mult_out <= (others => '0');
	--o_mult_out <= std_logic_vector(unsigned(i_input_feature)*unsigned(i_filter));

end Behavioral;
