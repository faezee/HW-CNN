
library IEEE;
use ieee.std_logic_1164.all;
use work.my_pack.all;


entity Row is 
	port(
		o_GB_start_addr : out std_logic_vector(c_addr_width-1 downto 0);
		o_GB_addr_valid : out std_logic;
		o_GB_addr_burst_valid : out std_logic;
		i_GB_data_ready : in std_logic;
		i_GB_data : in std_logic_vector(c_data_width-1 downto 0);
		o_GB_end_burst : out std_logic;
		--
		o_MRC_ld_filter_done : out std_logic;
		--
		i_start_ld_input : in std_logic;
		i_start_ld_filter : in std_logic;
		clk : in std_logic;
		rst : in std_logic
	);
end Row;

architecture Behavioral of Row is 
	component Row_Controller
		port(
			o_LB_data_valid : out std_logic;			-- tell the Line_Buffer that the data is valid
			o_LB_wr_index   : out std_logic_vector(c_LB_index_length-1 downto 0);
			o_LB_rd_enable	: out std_logic;	--	tell the LB that it is about to be readen :D 
			o_LB_rd_index	: out std_logic_vector(c_LB_index_length-1 downto 0);	--	the index that LB should be readen,  with o_LB_rd_enable
			
			--	higher memory level
			o_start_addr : out std_logic_vector(c_addr_width-1 downto 0);
			o_GB_burst_addr_valid : inout std_logic;							-- is inout okay? :D
			o_GB_addr_valid : inout std_logic;										
			o_GB_end_burst : out std_logic;
			i_GB_data_ready : in std_logic;
			--	PE
			--o_PE_filter_valid : out std_logic;		
			o_PE_filter_valid : out std_logic_vector(c_PE_row_dim-1 downto 0);	--	tell the PE to store the buffer,
			o_PE_input_valid : out std_logic;
			i_mult_done : in std_logic;				--	consider again - if it's better that PE tells RC the mult is done.
			o_PE_LB_rd_index : out std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
			--  MRC
			o_load_filter_done : out std_logic;
			--
			clk : in std_logic;
			rst : in std_logic;
			i_start_ld_input : in std_logic;
			i_start_ld_filter : in std_logic
		);
	end component;

	component Line_Buffer 
		port(
			i_rd_enable : in std_logic;
			i_rd_index : in std_logic_vector(c_LB_index_length-1 downto 0); 
			o_rd_data : out std_logic_vector(c_data_width-1 downto 0);
			i_wr_data_valid : in std_logic;
			i_wr_data : in std_logic_vector(c_data_width-1 downto 0);
			i_wr_index : in std_logic_vector(c_LB_index_length-1 downto 0);
			clk : in std_logic;
			rst : in std_logic
		);
	end component;

	component Global_Buffer
		port(
			i_start_addr : in std_logic_vector(c_addr_width-1 downto 0);
			i_addr_valid : in std_logic;
			i_addr_burst_valid : in std_logic;
			o_data_ready : out std_logic;
			o_data : out std_logic_vector(c_data_width-1 downto 0);
			i_end_burst : in std_logic;
			clk : in std_logic;
			rst : in std_logic
		);
	end component;

	component PE 
		generic(
			g_PE_number : integer := 0
		);
		port(
			i_input_data : std_logic_vector(c_data_width-1 downto 0);
			i_filter_valid : in std_logic;
			i_filter_data : std_logic_vector(c_data_width-1 downto 0);
			i_input_valid : in std_logic;
			o_mult_done : out std_logic;
			i_buff_rd_index : in std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
			clk : in std_logic;
			rst : in std_logic
		);
	end component;

	signal  w_addr_valid_RC_GB : std_logic;
	signal  w_addr_valid_burst_RC_GB : std_logic;
	signal  w_start_addr_RC_GB : std_logic_vector(c_addr_width-1 downto 0);
	signal  w_data_valid_RC_LB : std_logic;
	signal  w_wr_index_RC_LB : std_logic_vector(c_LB_index_length-1 downto 0);
	signal  w_data_ready_GB_RC : std_logic;
	signal  w_data_GB_LB_PE : std_logic_vector(c_data_width -1 downto 0);		-- data from GB to PE and LB
	--
	signal  w_filt_val_RC_PE_GB : std_logic;
	signal  w_filt_val_RC_PE : std_logic_vector(c_PE_row_dim-1 downto 0);
	
	signal  w_rd_data_LB_PE		: std_logic_vector(c_data_width-1 downto 0);
	signal  w_rd_enable_RC_LB	: std_logic;
	signal  w_rd_index_RC_LB	: std_logic_vector(c_LB_index_length-1 downto 0);
	signal  w_rd_index_RC_PE  : std_logic_vector(c_PE_buff_idx_lngth-1 downto 0);
	signal  w_mult_done_PE_RC	: std_logic;
	signal  w_input_valid_RC_PE	: std_logic;
	signal  w_end_burst_RC_GB : std_logic;
	
	-- test
	--signal  r_MRC_start : std_logic := '0';
	--signal  clk : std_logic := '0';
	--signal  rst : std_logic := '0';
	--
	--constant half_period : time := 5ps;
	--shared variable simend : boolean := false;


begin

	------------------------------------
	RC_ins : Row_Controller
	port map (
		o_LB_data_valid => w_data_valid_RC_LB,
		o_LB_wr_index => w_wr_index_RC_LB,
		o_LB_rd_enable => w_rd_enable_RC_LB,
		o_LB_rd_index => w_rd_index_RC_LB,
		--
		o_start_addr => w_start_addr_RC_GB,
		o_GB_burst_addr_valid => w_addr_valid_burst_RC_GB,			-- is inout okay? :D
		o_GB_addr_valid => w_addr_valid_RC_GB,
		o_GB_end_burst => w_end_burst_RC_GB,
		i_GB_data_ready => w_data_ready_GB_RC,
		--
		o_PE_filter_valid => w_filt_val_RC_PE,						--	tell the PE to store the buffer,
		o_PE_input_valid => w_input_valid_RC_PE,
		i_mult_done => w_mult_done_PE_RC,
		o_PE_LB_rd_index => w_rd_index_RC_PE,
		--
		o_load_filter_done => o_MRC_ld_filter_done,
		--
		clk => clk,
		rst => rst,
		i_start_ld_input => i_start_ld_input,
		i_start_ld_filter => i_start_ld_filter
	);
	------------------------------
	LB_ins : Line_Buffer
	port map(
		i_rd_enable => w_rd_enable_RC_LB,
		i_rd_index => w_rd_index_RC_LB,
		o_rd_data => w_rd_data_LB_PE,
		i_wr_data_valid => w_data_valid_RC_LB,
		i_wr_data => w_data_GB_LB_PE,
		i_wr_index => w_wr_index_RC_LB,
		clk => clk,
		rst => rst
	);
	
	-----------------------------------
	--	ports to and from Global_Buffer
	o_GB_start_addr <= w_start_addr_RC_GB;
	o_GB_addr_valid <= w_addr_valid_RC_GB;
	o_GB_addr_burst_valid <= w_addr_valid_burst_RC_GB;
	o_GB_end_burst <= w_end_burst_RC_GB;
	
	w_data_ready_GB_RC <= i_GB_data_ready;
	w_data_GB_LB_PE <= i_GB_data;	
	
	------------------------------------------------------------
	PE_gen : for I in 0 to c_PE_row_dim-1 generate
		PE_ins : PE
		generic map(
			g_PE_number => I
		)
		port map(
			i_input_valid => w_input_valid_RC_PE,
			i_input_data => w_rd_data_LB_PE,
			o_mult_done => w_mult_done_PE_RC,
			i_filter_data => w_data_GB_LB_PE,
			i_filter_valid => w_filt_val_RC_PE(I),
			i_buff_rd_index => w_rd_index_RC_PE,
			clk => clk,
			rst => rst
		);
	end generate;

	-----------------------------------------------------------------------------------------
	-----------------------------------------------------------------------------------------
	-----------------------------------------------------------------------------------------
	-----------------------------------------------------------------------------------------
	-----------------------------------------------------------------------------------------
	-----------------------------------------------------------------------------------------

	


end Behavioral;
