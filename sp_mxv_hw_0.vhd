library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use ieee.std_logic_unsigned.all;

library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;

entity sp_mxv_hw is
    generic(
			-- AXI slave generics
			C_S_AXI_DATA_WIDTH             	: integer              	:= 32;
			C_S_AXI_ADDR_WIDTH             	: integer              	:= 32;
			C_S_AXI_ID_WIDTH               	: integer              	:= 4;
			C_S_AXI_BASEADDR          			: std_logic_vector     	:= X"FFFFFFFF";
			C_S_AXI_HIGHADDR          			: std_logic_vector     	:= X"00000000";
			C_S_AXI_BURST_LEN						: integer					:= 256;
			C_S0_AXIS_TDATA_WIDTH				: integer					:= 32;
			C_M0_AXIS_TDATA_WIDTH				: integer					:= 32;
			C_S1_AXIS_TDATA_WIDTH				: integer					:= 32;
			C_S2_AXIS_TDATA_WIDTH				: integer					:= 32;
			C_S3_AXIS_TDATA_WIDTH				: integer					:= 32;
--			C_M1_AXIS_TDATA_WIDTH				: integer					:= 32;
        P : integer := 2; -- number of processing elements
        MAX_SIZE : integer := 1024;  -- max matrix rows, based on the number of available memory (MUST BE a number of base 2)
        DATA_WIDTH : integer := 32; -- single precision floating point = 32 ; double precision floating point = 64
        P_FIFOS : integer := 32; -- FIFO length to use in each processor as input buffers
        PACKAGER_FIFOS : integer := 32 -- FIFO length to use as input buffers
    );
    Port(
     
        GLOBAL_RST : in std_logic;
									
			-- AXI slave ports
			S_AXI_ACLK             			: in  std_logic; --
			S_AXI_ARESETN                 : in  std_logic; --
			S_AXI_AWADDR                  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0); --
			S_AXI_AWVALID                 : in  std_logic; --
			S_AXI_WDATA                   : in  std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0); --
			S_AXI_WSTRB                   : in  std_logic_vector((C_S_AXI_DATA_WIDTH/8)-1 downto 0); --
			S_AXI_WVALID                  : in  std_logic; --
			S_AXI_BREADY                  : in  std_logic; -- not used, always valid response
			S_AXI_ARADDR                  : in  std_logic_vector(C_S_AXI_ADDR_WIDTH-1 downto 0); --
			S_AXI_ARVALID                 : in  std_logic; --
			S_AXI_RREADY                  : in  std_logic; --
			S_AXI_ARREADY                 : out std_logic; --
			S_AXI_RDATA                   : out std_logic_vector(C_S_AXI_DATA_WIDTH-1 downto 0); --
			S_AXI_RRESP                   : out std_logic_vector(1 downto 0); --
			S_AXI_RVALID                  : out std_logic; --
			S_AXI_WREADY                  : out std_logic; --
			S_AXI_BRESP                   : out std_logic_vector(1 downto 0); --
			S_AXI_BVALID                  : out std_logic; --
			S_AXI_AWREADY                 : out std_logic; --
			S_AXI_AWID                    : in  std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0); -- 
			S_AXI_AWLEN                   : in  std_logic_vector(ceil_log2(C_S_AXI_BURST_LEN)-1 downto 0); --
			S_AXI_AWSIZE                  : in  std_logic_vector(2 downto 0); -- not have use for now
			S_AXI_AWBURST                 : in  std_logic_vector(1 downto 0); -- not have use for now
			S_AXI_AWLOCK                  : in  std_logic; -- not have use for now
			S_AXI_AWCACHE                 : in  std_logic_vector(3 downto 0); -- not have use for now
			S_AXI_AWPROT                  : in  std_logic_vector(2 downto 0); -- not have use for now
			S_AXI_WLAST                   : in  std_logic; --
			S_AXI_BID                     : out std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0); --
			S_AXI_ARID                    : in  std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0); --
			S_AXI_ARLEN                   : in  std_logic_vector(ceil_log2(C_S_AXI_BURST_LEN)-1 downto 0); --
			S_AXI_ARSIZE                  : in  std_logic_vector(2 downto 0); --
			S_AXI_ARBURST                 : in  std_logic_vector(1 downto 0); --
			S_AXI_ARCACHE                 : in  std_logic_vector(3 downto 0); --
			S_AXI_ARPROT                  : in  std_logic_vector(2 downto 0); --
			S_AXI_ARLOCK                  : in  std_logic; -- not have use for now
			S_AXI_RID                     : out std_logic_vector(C_S_AXI_ID_WIDTH-1 downto 0); -- 
			S_AXI_RLAST                   : out std_logic; --	
	
        -- AXI STREAM interface 0
			S0_AXIS_TREADY						: out	std_logic;
			S0_AXIS_TDATA						: in	std_logic_vector(C_S0_AXIS_TDATA_WIDTH-1 downto 0);
			S0_AXIS_TLAST						: in	std_logic;
			S0_AXIS_TVALID						: in	std_logic;
			M0_AXIS_TVALID						: out	std_logic;
			M0_AXIS_TDATA						: out	std_logic_vector(C_M0_AXIS_TDATA_WIDTH-1 downto 0);
			M0_AXIS_TLAST						: out	std_logic;
			M0_AXIS_TREADY						: in	std_logic;
			-- AXI STREAM interface 1
			S1_AXIS_TREADY						: out	std_logic;
			S1_AXIS_TDATA						: in	std_logic_vector(C_S1_AXIS_TDATA_WIDTH-1 downto 0);
			S1_AXIS_TLAST						: in	std_logic;
			S1_AXIS_TVALID						: in	std_logic;
--			M1_AXIS_TVALID						: out	std_logic;
--			M1_AXIS_TDATA						: out	std_logic_vector(C_M1_AXIS_TDATA_WIDTH-1 downto 0);
--			M1_AXIS_TLAST						: out	std_logic;
--			M1_AXIS_TREADY						: in	std_logic;
			-- AXI STREAM interface 2
			S2_AXIS_TREADY						: out	std_logic;
			S2_AXIS_TDATA						: in	std_logic_vector(C_S2_AXIS_TDATA_WIDTH-1 downto 0);
			S2_AXIS_TLAST						: in	std_logic;
			S2_AXIS_TVALID						: in	std_logic;
			-- AXI STREAM interface 3
			S3_AXIS_TREADY						: out	std_logic;
			S3_AXIS_TDATA						: in	std_logic_vector(C_S3_AXIS_TDATA_WIDTH-1 downto 0);
			S3_AXIS_TLAST						: in	std_logic;
			S3_AXIS_TVALID						: in	std_logic
	
    );
end sp_mxv_hw;

architecture Behavioral of sp_mxv_hw is

	
	COMPONENT axi_slave_wrapper is
	GENERIC
	(
		S_AXI_DATA_WIDTH             	: integer              := 32;
		S_AXI_ADDR_WIDTH             	: integer              := 32;
		S_AXI_ID_WIDTH               	: integer              := 4
	);
   PORT
	(
		 RST 							  : in  std_logic; -- reset active to high
		 ----------------------------------------------------------------------------------
		 ACLK                     : in  std_logic; --
		 ARESETN                  : in  std_logic; --
		 AWADDR                   : in  std_logic_vector(S_AXI_ADDR_WIDTH-1 downto 0); --
		 AWVALID                  : in  std_logic; --
		 WDATA                    : in  std_logic_vector(S_AXI_DATA_WIDTH-1 downto 0); --
		 WSTRB                    : in  std_logic_vector((S_AXI_DATA_WIDTH/8)-1 downto 0); --
		 WVALID                   : in  std_logic; --
		 BREADY                   : in  std_logic; -- not used, always valid response
		 ARADDR                   : in  std_logic_vector(S_AXI_ADDR_WIDTH-1 downto 0); --
		 ARVALID                  : in  std_logic; --
		 RREADY                   : in  std_logic; --
		 ARREADY                  : out std_logic; --
		 RDATA                    : out std_logic_vector(S_AXI_DATA_WIDTH-1 downto 0); --
		 RRESP                    : out std_logic_vector(1 downto 0); --
		 RVALID                   : out std_logic; --
		 WREADY                   : out std_logic; --
		 BRESP                    : out std_logic_vector(1 downto 0); --
		 BVALID                   : out std_logic; --
		 AWREADY                  : out std_logic; --
		 AWID                     : in  std_logic_vector(S_AXI_ID_WIDTH-1 downto 0); -- 
		 AWLEN                    : in  std_logic_vector(7 downto 0); --
		 AWSIZE                   : in  std_logic_vector(2 downto 0); -- not have use for now
		 AWBURST                  : in  std_logic_vector(1 downto 0); -- not have use for now
		 AWLOCK                   : in  std_logic; -- not have use for now
		 AWCACHE                  : in  std_logic_vector(3 downto 0); -- not have use for now
		 AWPROT                   : in  std_logic_vector(2 downto 0); -- not have use for now
		 WLAST                    : in  std_logic; --
		 BID                      : out std_logic_vector(S_AXI_ID_WIDTH-1 downto 0); --
		 ARID                     : in  std_logic_vector(S_AXI_ID_WIDTH-1 downto 0); --
		 ARLEN                    : in  std_logic_vector(7 downto 0); --
		 ARSIZE                   : in  std_logic_vector(2 downto 0); --
		 ARBURST                  : in  std_logic_vector(1 downto 0); --
		 ARCACHE                  : in  std_logic_vector(3 downto 0); --
		 ARPROT                   : in  std_logic_vector(2 downto 0); --
		 RID                      : out std_logic_vector(S_AXI_ID_WIDTH-1 downto 0); -- 
		 RLAST                    : out std_logic; --
		 
		 NNZ_PKG2PE :  in std_logic_vector(31 downto 0);
		 STM_STATUS : in std_logic_vector(31 downto 0);
		 CTRL_REGS_DOUT			  : out slv_reg_type
	);
	END COMPONENT;
	
signal instruction : std_logic_vector(ceil_log2(P)+3*DATA_WIDTH downto 0);
signal instruction_valid, processors_not_ready, processors_ready, data_valid, data_empty, finished : std_logic := '0';
signal code : std_logic_vector(ceil_log2(P) downto 0) := (others => '0');
signal data_return : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');
signal reset : std_logic := '1';

signal zero : std_logic := '0';
signal zeros : std_logic_vector(DATA_WIDTH-1 downto 0) := (others => '0');

-- sinais para o for generate 1 para P
type VEC1 is array (P downto 0) of std_logic_vector(ceil_log2(P) downto 0);
signal code_geral : VEC1 := (others => (others => '0'));

-- sinais para o for generate P para 1
type VEC2 is array (P downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
signal stop_geral : std_logic_vector(P downto 0) := (others => '0');
signal data_geral : VEC2 := (others => (others => '0'));
signal valid_geral : std_logic_vector(P downto 0) := (others => '0');
signal empty_geral : std_logic_vector(P downto 0) := (others => '0');
signal rst_geral : std_logic;

signal ctrl_regs : slv_reg_type;

signal NNZ_PKG2PE_temp : std_logic_vector(31 downto 0);
signal STM_STATUS_temp : std_logic_vector(31 downto 0);
		  
begin       
--	 S0_AXIS_TREADY <= '1';
--	 S1_AXIS_TREADY <= '1';
--	 S2_AXIS_TREADY <= '1';
--	 S3_AXIS_TREADY <= '1';
	 
    empacotador: packager
    generic map(
        P => P,
        CODE_BITS => ceil_log2(P),
        SIZE => MAX_SIZE/P,
        INPUT_FIFO_LENGTH => PACKAGER_FIFOS,
        DATA_WIDTH => DATA_WIDTH
    )
    Port map(
        clk => S_AXI_ACLK, --clk,
        rst => reset,
        reset_processor_system => rst_geral,
	
        s_axis_tdata_0 => S0_AXIS_TDATA,
        s_axis_tvalid_0 => S0_AXIS_TVALID,
        s_axis_tready_0 => S0_AXIS_TREADY,
        s_axis_tlast_0 => S0_AXIS_TLAST,
        
        s_axis_tdata_1 => S1_AXIS_TDATA,
        s_axis_tvalid_1 => S1_AXIS_TVALID,
        s_axis_tready_1 => S1_AXIS_TREADY,
        s_axis_tlast_1 => S1_AXIS_TLAST,
		  
		  s_axis_tdata_2 => S2_AXIS_TDATA,
        s_axis_tvalid_2 => S2_AXIS_TVALID,
        s_axis_tready_2 => S2_AXIS_TREADY,
        s_axis_tlast_2 => S2_AXIS_TLAST,
		  
		  s_axis_tdata_3 => S3_AXIS_TDATA,
        s_axis_tvalid_3 => S3_AXIS_TVALID,
        s_axis_tready_3 => S3_AXIS_TREADY,
        s_axis_tlast_3 => S3_AXIS_TLAST,
        
        m_axis_tdata => M0_AXIS_TDATA,
        m_axis_tvalid => M0_AXIS_TVALID,
        m_axis_tready => M0_AXIS_TREADY,
        m_axis_tlast => M0_AXIS_TLAST,
        
--		  columns_total => ctrl_regs(0), --columns_test,
        rows_total => ctrl_regs(1), --rows_test,
		  
        instruction => instruction,
        valid => instruction_valid,
        ready => processors_ready,
        
        code => code_geral(P),
        
        data_return => data_geral(P),
        data_valid => valid_geral(P),
        data_empty => empty_geral(P),
		  
		  NNZ_PKG2PE => NNZ_PKG2PE_temp,
		  STM_STATUS => STM_STATUS_temp
    );
    
    processor_array: for i in 0 to P-1 generate
    begin
    
        processors: processor
        generic map(
            P => P,
            LOG_P => ceil_log2(P),
            DATA_WIDTH => DATA_WIDTH,
            FIFO_LENGTH => P_FIFOS,
            MEM_SIZE => MAX_SIZE/P
        )
        Port map(         
            clk => S_AXI_ACLK, --clk,
            rst => rst_geral,
                   
            instruction_in => instruction,
            instruction_valid_in => instruction_valid,
            
            instruction_stop_in => stop_geral(i),
            instruction_stop_out => stop_geral(i+1),
            
            code_in => code_geral(P-i),
            code_out => code_geral(P-1-i),
         
            datain => data_geral(i),
            validin => valid_geral(i),
            emptyin => empty_geral(i),
            
            dataout => data_geral(i+1),
            validout => valid_geral(i+1),
            emptyout => empty_geral(i+1)
        );
        end generate;
        
    processors_ready <= not stop_geral(P);
    reset <= GLOBAL_RST; --not rst
    
	 Inst_AXI_SLV_WRAPPER: axi_slave_wrapper
	GENERIC MAP
	(
		S_AXI_DATA_WIDTH => C_S_AXI_DATA_WIDTH,
		S_AXI_ADDR_WIDTH => C_S_AXI_ADDR_WIDTH,
		S_AXI_ID_WIDTH => C_S_AXI_ID_WIDTH
	)
	PORT MAP
	(
		 RST => GLOBAL_RST,
		 -------------------------------------------------------------------------------------
		 -- AXI SLAVE RELATED SIGNALS
		 ACLK => S_AXI_ACLK,
		 ARESETN => S_AXI_ARESETN,
		 AWADDR => S_AXI_AWADDR, 
		 AWVALID => S_AXI_AWVALID,
		 WDATA => S_AXI_WDATA,
		 WSTRB => S_AXI_WSTRB,
		 WVALID => S_AXI_WVALID,
		 BREADY => S_AXI_BREADY,
		 ARADDR => S_AXI_ARADDR,
		 ARVALID => S_AXI_ARVALID,
		 RREADY => S_AXI_RREADY,
		 ARREADY => S_AXI_ARREADY,
		 RDATA => S_AXI_RDATA,
		 RRESP => S_AXI_RRESP,
		 RVALID => S_AXI_RVALID,
		 WREADY => S_AXI_WREADY,
		 BRESP => S_AXI_BRESP,
		 BVALID => S_AXI_BVALID,
		 AWREADY => S_AXI_AWREADY,
		 AWID => S_AXI_AWID,
		 AWLEN => S_AXI_AWLEN,
		 AWSIZE => S_AXI_AWSIZE,
		 AWBURST => S_AXI_AWBURST,
		 AWLOCK => S_AXI_AWLOCK,
		 AWCACHE => S_AXI_AWCACHE,
		 AWPROT => S_AXI_AWPROT,
		 WLAST => S_AXI_WLAST,
		 BID => S_AXI_BID,
		 ARID => S_AXI_ARID,
		 ARLEN => S_AXI_ARLEN,
		 ARSIZE => S_AXI_ARSIZE,
		 ARBURST => S_AXI_ARBURST,
		 ARCACHE => S_AXI_ARCACHE,
		 ARPROT => S_AXI_ARPROT,
		 RID => S_AXI_RID,
		 RLAST => S_AXI_RLAST,
		 -- CONTROLER RELATED SIGNALS		 
		 NNZ_PKG2PE => NNZ_PKG2PE_temp,
		 STM_STATUS => STM_STATUS_temp,		 
		 CTRL_REGS_DOUT => ctrl_regs			  
	);
	
end Behavioral;
