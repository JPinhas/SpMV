----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    14:21:31 02/13/2015 
-- Design Name: 
-- Module Name:    axi_slave_wrapper - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use ieee.numeric_std.all; -- used for the shifter

library mylibs_v1_00_a;
use mylibs_v1_00_a.PE_Pkg.all;
use mylibs_v1_00_a.float_pkg.all;
use mylibs_v1_00_a.SoC_Pkg.all;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity axi_slave_wrapper is
generic
(
   S_AXI_DATA_WIDTH             	: integer              := 32;
   S_AXI_ADDR_WIDTH             	: integer              := 32;
   S_AXI_ID_WIDTH               	: integer              := 4
    -- DO NOT EDIT ABOVE THIS LINE ---------------------
  );
  port
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
	 
	 NNZ_PKG2PE 					: in std_logic_vector(31 downto 0);
    STM_STATUS 					: in std_logic_vector(31 downto 0);
	 CTRL_REGS_DOUT			  : out slv_reg_type
);
end axi_slave_wrapper;

architecture Behavioral of axi_slave_wrapper is

	COMPONENT AXI_SLV_WR_STM
	GENERIC 
	(
		AXI_DATA_WIDTH             : integer              := 32;
		AXI_ADDR_WIDTH             : integer              := 32;
		AXI_ID_WIDTH               : integer              := 4
	);
	PORT(
		ACLK : IN std_logic;
		ARESETN : IN std_logic;
		AXI_AWADDR : IN std_logic_vector(AXI_ADDR_WIDTH-1 downto 0);
		AXI_AWVALID : IN std_logic;
		AXI_WDATA : IN std_logic_vector(AXI_DATA_WIDTH-1 downto 0);
		AXI_WSTRB : IN std_logic_vector(3 downto 0);
		AXI_WVALID : IN std_logic;
		AXI_BREADY : IN std_logic;
		AXI_AWID : IN std_logic_vector(AXI_ID_WIDTH-1 downto 0);
		AXI_AWLEN : IN std_logic_vector(7 downto 0);
		AXI_AWSIZE : IN std_logic_vector(2 downto 0);
		AXI_AWBURST : IN std_logic_vector(1 downto 0);
		AXI_AWLOCK : IN std_logic;
		AXI_AWCACHE : IN std_logic_vector(3 downto 0);
		AXI_AWPROT : IN std_logic_vector(2 downto 0);
		AXI_WLAST : IN std_logic;          
		AXI_WREADY : OUT std_logic;
		AXI_BRESP : OUT std_logic_vector(1 downto 0);
		AXI_BVALID : OUT std_logic;
		AXI_AWREADY : OUT std_logic;
		AXI_BID : OUT std_logic_vector(AXI_ID_WIDTH-1 downto 0);
		
		FIFO_FULL : IN std_logic;
		WE_MEM_SPACE : OUT std_logic;
		DATAOUT : OUT std_logic_vector(AXI_DATA_WIDTH-1 downto 0)
		);
	END COMPONENT;
	
	COMPONENT AXI_SLV_RD_STM
	GENERIC 
	(
		AXI_DATA_WIDTH             : integer              := 32;
		AXI_ADDR_WIDTH             : integer              := 32;
		AXI_ID_WIDTH               : integer              := 4 
	);
	PORT(
		DATAIN : IN std_logic_vector(AXI_DATA_WIDTH-1 downto 0);
		FIFO_VALID : IN std_logic;
		RE_MEM_SPACE : OUT std_logic;
		ACLK : IN std_logic;
		ARESETN : IN std_logic;
		AXI_ARADDR : IN std_logic_vector(AXI_ADDR_WIDTH-1 downto 0);
		AXI_ARVALID : IN std_logic;
		AXI_RREADY : IN std_logic;
		AXI_ARID : IN std_logic_vector(AXI_ID_WIDTH-1 downto 0);
		AXI_ARLEN : IN std_logic_vector(7 downto 0);
		AXI_ARSIZE : IN std_logic_vector(2 downto 0);
		AXI_ARBURST : IN std_logic_vector(1 downto 0);
		AXI_ARCACHE : IN std_logic_vector(3 downto 0);
		AXI_ARPROT : IN std_logic_vector(2 downto 0);          
		AXI_ARREADY : OUT std_logic;
		AXI_RDATA : OUT std_logic_vector(AXI_DATA_WIDTH-1 downto 0);
		AXI_RRESP : OUT std_logic_vector(1 downto 0);
		AXI_RVALID : OUT std_logic;
		AXI_RID : OUT std_logic_vector(AXI_ID_WIDTH-1 downto 0);
		AXI_RLAST : OUT std_logic
		);
	END COMPONENT;
	
	COMPONENT slave_interface
	PORT(
		CLK : IN std_logic;
		RST : IN std_logic;
		PROC_WE : IN std_logic;
		CTRL_WE : IN std_logic;
		CTRL_RE : IN std_logic;
		ARADDR : IN std_logic_vector(AXI_SLV_AWIDTH-1 downto 0);
		AWADDR : IN std_logic_vector(AXI_SLV_AWIDTH-1 downto 0);
		PROC_WDATA : IN std_logic_vector(AXI_SLV_DWIDTH-1 downto 0); --
		NNZ_PKG2PE 					: in std_logic_vector(31 downto 0);
      STM_STATUS 					: in std_logic_vector(31 downto 0);
		CTRL_RDATA : OUT slv_reg_type;
		PROC_RDATA : OUT std_logic_vector(AXI_SLV_DWIDTH-1 downto 0)
		);
	END COMPONENT;
	
	signal storage_we : std_logic;
	signal storage_re : std_logic;
	signal fifo_full	: std_logic;
	signal fifo_valid	: std_logic_vector(1 downto 0);
	signal storage_din : std_logic_vector(AXI_SLV_DWIDTH-1 downto 0);
	signal storage_dout : std_logic_vector(AXI_SLV_DWIDTH-1 downto 0);

begin
	 
	Inst_AXI_SLV_WR_STM: AXI_SLV_WR_STM 
	GENERIC MAP 
	(
		AXI_DATA_WIDTH => S_AXI_DATA_WIDTH,
		AXI_ADDR_WIDTH => S_AXI_ADDR_WIDTH,
		AXI_ID_WIDTH => S_AXI_ID_WIDTH
	)
	PORT MAP(
		ACLK => ACLK,
		ARESETN => ARESETN,
		AXI_AWADDR => AWADDR,
		AXI_AWVALID => AWVALID,
		AXI_WDATA => WDATA,
		AXI_WSTRB => WSTRB,
		AXI_WVALID => WVALID,
		AXI_BREADY => BREADY,
		AXI_WREADY => WREADY,
		AXI_BRESP => BRESP,
		AXI_BVALID => BVALID,
		AXI_AWREADY => AWREADY,
		AXI_AWID => AWID,
		AXI_AWLEN => AWLEN,
		AXI_AWSIZE => AWSIZE,
		AXI_AWBURST => AWBURST,
		AXI_AWLOCK => AWLOCK,
		AXI_AWCACHE => AWCACHE,
		AXI_AWPROT => AWPROT,
		AXI_WLAST => WLAST,
		AXI_BID => BID,
		
		FIFO_FULL => '0',
		WE_MEM_SPACE => storage_we,
		DATAOUT => storage_din
	);

	Inst_AXI_SLV_RD_STM: AXI_SLV_RD_STM 
	GENERIC MAP 
	(
		AXI_DATA_WIDTH => S_AXI_DATA_WIDTH,
		AXI_ADDR_WIDTH => S_AXI_ADDR_WIDTH,
		AXI_ID_WIDTH => S_AXI_ID_WIDTH             
	)
	PORT MAP(
		DATAIN => storage_dout,
		FIFO_VALID => '1', -- reads available registers --fifo_valid,
		RE_MEM_SPACE => open, -- we only read registers -- storage_re,
		ACLK => ACLK,
		ARESETN => ARESETN,
		AXI_ARADDR => ARADDR,
		AXI_ARVALID => ARVALID,
		AXI_RREADY => RREADY,
		AXI_ARREADY => ARREADY,
		AXI_RDATA => RDATA,
		AXI_RRESP => RRESP,
		AXI_RVALID => RVALID,
		AXI_ARID => ARID,
		AXI_ARLEN => ARLEN,
		AXI_ARSIZE => ARSIZE,
		AXI_ARBURST => ARBURST,
		AXI_ARCACHE => ARCACHE,
		AXI_ARPROT => ARPROT,
		AXI_RID => RID,
		AXI_RLAST => RLAST
	);

		Inst_slave_interface: slave_interface 
		PORT MAP(
			CLK => ACLK,
			RST => RST,
			PROC_WE => storage_we,
			CTRL_WE => '1',
			CTRL_RE => '0',
			ARADDR => ARADDR,
			AWADDR => AWADDR,
			PROC_WDATA => storage_din,
			NNZ_PKG2PE => NNZ_PKG2PE,
			STM_STATUS => STM_STATUS,
			CTRL_RDATA => CTRL_REGS_DOUT,
			PROC_RDATA => storage_dout
	);	
	
end Behavioral;
