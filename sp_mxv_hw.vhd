library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.MATH_REAL.ALL;
use ieee.numeric_std.all;
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
			C_S2_AXIS_TDATA_WIDTH				: integer					:= 32;
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
--			M1_AXIS_TVALID						: out	std_logic;
--			M1_AXIS_TDATA						: out	std_logic_vector(C_M1_AXIS_TDATA_WIDTH-1 downto 0);
--			M1_AXIS_TLAST						: out	std_logic;
--			M1_AXIS_TREADY						: in	std_logic;
			-- AXI STREAM interface 2
			S2_AXIS_TREADY						: out	std_logic;
			S2_AXIS_TDATA						: in	std_logic_vector(C_S2_AXIS_TDATA_WIDTH-1 downto 0);
			S2_AXIS_TLAST						: in	std_logic;
			S2_AXIS_TVALID						: in	std_logic;
			
			switches								: in std_logic_vector(7 downto 0);
			leds 									: out std_logic_vector(7 downto 0)
	
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
	
type VEC is array (P downto 0) of std_logic_vector(ceil_log2(P) downto 0);
type VEC2 is array (P downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);
type VEC3 is array (255 downto 0) of std_logic_vector(7 downto 0);
type VEC4 is array (P-1 downto 0) of std_logic_vector(7 downto 0);
type VEC5 is array (P-1 downto 0) of std_logic_vector(DATA_WIDTH-1 downto 0);

signal instruction : std_logic_vector(ceil_log2(P)+3*DATA_WIDTH downto 0);
signal instruction_valid, processors_not_ready, processors_ready, data_empty, finished, reset, zero, rst_geral, packager_finished : std_logic;
signal code : std_logic_vector(ceil_log2(P) downto 0);
signal data_return, zeros : std_logic_vector(DATA_WIDTH-1 downto 0);
signal code_geral : VEC;
signal contagem_packager : VEC1;
signal data_geral : VEC2;
signal stop_geral, valid_geral, finished_geral : std_logic_vector(P downto 0);
signal ctrl_regs : slv_reg_type;
signal NNZ_PKG2PE_temp : std_logic_vector(31 downto 0);
signal STM_STATUS_temp : std_logic_vector(31 downto 0);

-- debug signals
signal debug : VEC3;
signal states : VEC4;
signal colisoes : VEC5;
signal contador_x_curr, contador_x_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal contador_val_curr, contador_val_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal contador_ind_curr, contador_ind_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal contador_ptr_curr, contador_ptr_next : std_logic_vector(DATA_WIDTH-1 downto 0);
signal idle, idle_temp : std_logic_vector(DATA_WIDTH-1 downto 0);
signal fifos : std_logic_vector(3 downto 0);
--type TEST is array (P-1 downto 0) of processor_output;
--signal valores : TEST;

begin 
	 
    empacotador: packager
    generic map(
        P => P,
        CODE_BITS => ceil_log2(P),
        SIZE => MAX_SIZE/P,
        INPUT_FIFO_LENGTH => PACKAGER_FIFOS,
        DATA_WIDTH => DATA_WIDTH
    )
    Port map(
        clk => S_AXI_ACLK,
        rst => reset,
        reset_processor_system => rst_geral,
	
        s_axis_tdata_0 => S0_AXIS_TDATA,
        s_axis_tvalid_0 => S0_AXIS_TVALID,
        s_axis_tready_0 => S0_AXIS_TREADY,
        s_axis_tlast_0 => S0_AXIS_TLAST,
		  
		  s_axis_tdata_2 => S2_AXIS_TDATA,
        s_axis_tvalid_2 => S2_AXIS_TVALID,
        s_axis_tready_2 => S2_AXIS_TREADY,
        s_axis_tlast_2 => S2_AXIS_TLAST,
        
        m_axis_tdata => M0_AXIS_TDATA,
        m_axis_tvalid => M0_AXIS_TVALID,
        m_axis_tready => M0_AXIS_TREADY,
        m_axis_tlast => M0_AXIS_TLAST,
        
        rows_total => ctrl_regs(1),
		  
        instruction => instruction,
        valid => instruction_valid,
        ready => processors_ready,
        
        code => code_geral(0),
			idle => idle,
			fifos_state => fifos,
			contador_estados => contagem_packager,
        
        data_return => data_geral(0),
        data_valid => valid_geral(0),
		  
        packager_finished => packager_finished,
        processors_finished => finished_geral(0),
		  
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
            clk => S_AXI_ACLK,
            rst => rst_geral,
                   
            instruction_in => instruction,
            instruction_valid_in => instruction_valid,
            
            instruction_stop_in => stop_geral(P-i),
            instruction_stop_out => stop_geral(P-i-1),
            
            code_in => code_geral(i),
            code_out => code_geral(i+1),
            
            packager_finished => packager_finished,
		    state => states(i),
			collisions => colisoes(i),
			-- saida => valores(i),
		 
            datain => data_geral(P-i),
            dataout => data_geral(P-i-1),
            
            validin => valid_geral(P-i),
            validout => valid_geral(P-i-1),
            
            finishedin => finished_geral(P-i),
            finishedout => finished_geral(P-i-1)
        );
	end generate;
    
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
        
	processors_ready <= not stop_geral(P);
	reset <= GLOBAL_RST;

	stop_geral(P) <= '0';
	data_geral(P) <= (others => '0');
	valid_geral(P) <= '0';
	finished_geral(P) <= '1'; 
	
	leds <= debug(to_integer(unsigned(switches)));
	
	process(S_AXI_ACLK, GLOBAL_RST) is
    begin        
        if (GLOBAL_RST = '1') then
			contador_x_curr <= (others => '0');
			contador_ind_curr <= (others => '0');
			
        elsif (S_AXI_ACLK'event and S_AXI_ACLK = '1') then    
			contador_x_curr <= contador_x_next;
			contador_ind_curr <= contador_ind_next;
			
        end if;
    end process;
	
	process(S0_AXIS_TVALID, S2_AXIS_TVALID) is
    begin        
		-- defaults
		contador_x_next <= contador_x_curr;
		contador_ind_next <= contador_ind_curr;
			
        if (S0_AXIS_TVALID = '1') then  
			contador_x_next <= contador_x_curr + 1;
        end if;
			
        if (S2_AXIS_TVALID = '1') then  
			contador_ind_next <= contador_ind_curr + 1;
        end if;
		
    end process;
	
	------------------------------------------------------------
	-- colocar variaveis a fazer debug nas posicoes desejadas --
	------------------------------------------------------------
	
	 -- contador de dados que chegam à fifo de x
	 debug(0) <= contador_x_curr(7 downto 0);
	 debug(1) <= contador_x_curr(15 downto 8);
	 debug(2) <= contador_x_curr(23 downto 16);
	 debug(3) <= contador_x_curr(31 downto 24);
	
	 -- contador de dados que chegam à fifo de ind
	 debug(8) <= contador_ind_curr(7 downto 0);
	 debug(9) <= contador_ind_curr(15 downto 8);
	 debug(10) <= contador_ind_curr(23 downto 16);
	 debug(11) <= contador_ind_curr(31 downto 24);
	
	 -- estado final do processador 1
	 debug(16) <= states(0);
	
	 -- estado final do processador 2
	 debug(17) <= states(1);
	
	 -- colisoes do processador 1
	 debug(18) <= colisoes(0)(7 downto 0);
	 debug(19) <= colisoes(0)(15 downto 8);
	 debug(20) <= colisoes(0)(23 downto 16);
	 debug(21) <= colisoes(0)(31 downto 24);
	
	 -- colisoes do processador 2
	 debug(22) <= colisoes(1)(7 downto 0);
	 debug(23) <= colisoes(1)(15 downto 8);
	 debug(24) <= colisoes(1)(23 downto 16);
	 debug(25) <= colisoes(1)(31 downto 24);
	
	 -- idle cycles do packager
	 idle_temp <= idle - contador_ptr_curr - contador_val_curr;
	 debug(26) <= idle_temp(7 downto 0);
	 debug(27) <= idle_temp(15 downto 8);
	 debug(28) <= idle_temp(23 downto 16);
	 debug(29) <= idle_temp(31 downto 24);
	
	 -- estado de término de cada processador e das 4 fifos do packager
	 debug(30) <= finished_geral(0) & finished_geral(1) & "00" & fifos;
	
	 -- contadores de estados do packager
	 debug(31) <= contagem_packager(0)(7 downto 0);
	 debug(32) <= contagem_packager(0)(15 downto 8);
	 debug(33) <= contagem_packager(0)(23 downto 16);
	 debug(34) <= contagem_packager(0)(31 downto 24);
	
	 debug(35) <= contagem_packager(1)(7 downto 0);
	 debug(36) <= contagem_packager(1)(15 downto 8);
	 debug(37) <= contagem_packager(1)(23 downto 16);
	 debug(38) <= contagem_packager(1)(31 downto 24);
	
	 debug(39) <= contagem_packager(2)(7 downto 0);
	 debug(40) <= contagem_packager(2)(15 downto 8);
	 debug(41) <= contagem_packager(2)(23 downto 16);
	 debug(42) <= contagem_packager(2)(31 downto 24);

	 debug(43) <= contagem_packager(3)(7 downto 0);
	 debug(44) <= contagem_packager(3)(15 downto 8);
	 debug(45) <= contagem_packager(3)(23 downto 16);
	 debug(46) <= contagem_packager(3)(31 downto 24);
	
	 debug(47) <= contagem_packager(4)(7 downto 0);
	 debug(48) <= contagem_packager(4)(15 downto 8);
	 debug(49) <= contagem_packager(4)(23 downto 16);
	 debug(50) <= contagem_packager(4)(31 downto 24);
	
	 debug(51) <= contagem_packager(5)(7 downto 0);
	 debug(52) <= contagem_packager(5)(15 downto 8);
	 debug(53) <= contagem_packager(5)(23 downto 16);
	 debug(54) <= contagem_packager(5)(31 downto 24);

	 debug(55) <= contagem_packager(6)(7 downto 0);
	 debug(56) <= contagem_packager(6)(15 downto 8);
	 debug(57) <= contagem_packager(6)(23 downto 16);
	 debug(58) <= contagem_packager(6)(31 downto 24);
	
	 debug(59) <= contagem_packager(7)(7 downto 0);
	 debug(60) <= contagem_packager(7)(15 downto 8);
	 debug(61) <= contagem_packager(7)(23 downto 16);
	 debug(62) <= contagem_packager(7)(31 downto 24);
	
	 debug(63) <= contagem_packager(8)(7 downto 0);
	 debug(64) <= contagem_packager(8)(15 downto 8);
	 debug(65) <= contagem_packager(8)(23 downto 16);
	 debug(66) <= contagem_packager(8)(31 downto 24);
	
end Behavioral;
