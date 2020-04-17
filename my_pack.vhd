library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.MATH_REAL.ALL;

package my_pack is 

	constant  c_PE_row_dim : integer := 4;
	constant  c_PE_column_dim : integer := 4;


	constant  c_data_width :  integer := 8;
	constant  c_addr_width :  integer := 16;
	constant  c_mult_clk   :  integer := 5;				--	it takes g_mult_clk clocks to do the multiplying - (-1)
	constant  c_filter_dim_1 :	integer := 3;
	constant  c_filter_dim_2 :	integer := 3;
	constant  c_feature_dim_1 : integer := 5;
	constant  c_feature_dim_2 : integer := 5;
	constant  c_GB_depth  : integer := 128;
	constant  c_size_load_filter  : integer :=  c_filter_dim_1*c_filter_dim_2;
	constant  c_size_load_inputF  : integer :=  c_feature_dim_1*(c_filter_dim_2-1)+c_filter_dim_1;
	constant  c_Line_Buffer_depth : integer :=17;
	constant  c_PE_buff_idx_lngth : integer := 4;
	constant  c_PE_buff_depth : integer := 9;
	constant  c_buff_depth :	integer := 64;
	constant  c_index_length :	integer := 6;
	constant  c_fet_dim_1 : integer := 16;
	constant  c_LB_index_length : integer := 6;


	--	more than one row
	constant  c_mux_sel : integer := 2;				--	should be equal to log2(c_PE_column_dim)


	
	--type slv_array_t is array (natural range <>) of std_logic_vector;
	--type array_2D is array(integer range<>) of std_logic_vector(data_width-1 downto 0);
end package my_pack;
