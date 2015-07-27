----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:51:35 02/13/2015 
-- Design Name: 
-- Module Name:    AXI_SLV_WR_STM - Behavioral 
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

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity AXI_SLV_WR_STM is
generic
  (
    AXI_DATA_WIDTH             : integer              := 32;
    AXI_ADDR_WIDTH             : integer              := 32;
    AXI_ID_WIDTH               : integer              := 4
  );
  port
  (
    -- AXI RELATED BELLOW ---------------------
    ACLK                     : in  std_logic;
    ARESETN                  : in  std_logic;
    AXI_AWADDR                   : in  std_logic_vector(AXI_ADDR_WIDTH-1 downto 0);
    AXI_AWVALID                  : in  std_logic;
    AXI_WDATA                    : in  std_logic_vector(AXI_DATA_WIDTH-1 downto 0);
    AXI_WSTRB                    : in  std_logic_vector((AXI_DATA_WIDTH/8)-1 downto 0);
    AXI_WVALID                   : in  std_logic;
    AXI_BREADY                   : in  std_logic;
    AXI_WREADY                   : out std_logic;
    AXI_BRESP                    : out std_logic_vector(1 downto 0);
    AXI_BVALID                   : out std_logic;
    AXI_AWREADY                  : out std_logic;
    AXI_AWID                     : in  std_logic_vector(AXI_ID_WIDTH-1 downto 0);
    AXI_AWLEN                    : in  std_logic_vector(7 downto 0);
    AXI_AWSIZE                   : in  std_logic_vector(2 downto 0);
    AXI_AWBURST                  : in  std_logic_vector(1 downto 0);
    AXI_AWLOCK                   : in  std_logic;
    AXI_AWCACHE                  : in  std_logic_vector(3 downto 0);
    AXI_AWPROT                   : in  std_logic_vector(2 downto 0);
    AXI_WLAST                    : in  std_logic;
    AXI_BID                      : out std_logic_vector(AXI_ID_WIDTH-1 downto 0);
    -- AXI RELATED ABOVE  ---------------------
	 -- FROM REGISTER BANK ---------------------
	 FIFO_FULL							: in 	std_logic;
	 WE_MEM_SPACE						: out std_logic;
	 DATAOUT 							: out std_logic_vector(AXI_DATA_WIDTH-1 downto 0)
);
end AXI_SLV_WR_STM;

architecture Behavioral of AXI_SLV_WR_STM is

	type wr_fsm_states is (write_0, write_1); 
	signal wr_currstate, wr_nextstate: wr_fsm_states;
	
	signal awaddr : std_Logic_vector(AXI_ADDR_WIDTH-1 downto 0);
	signal wdata : std_Logic_vector(AXI_DATA_WIDTH-1 downto 0);
	signal awvalid : std_logic;
	signal awready : std_logic;
	signal wready : std_logic;
	signal wvalid : std_logic;
	signal bvalid : std_logic;
	signal bready : std_logic;
	signal wlast : std_logic;
	signal awlen : std_logic_vector(7 downto 0);
	signal bresp : std_logic_vector(1 downto 0);
	signal bid : std_logic_vector(AXI_ID_WIDTH-1 downto 0);
	signal wr_file_reg : std_logic;
	
begin

	--------------------------------------------------------------------------------------------
	-- INPUTS
	--------------------------------------------------------------------------------------------
   awaddr <= AXI_AWADDR;
	wdata <= AXI_WDATA;
--	wstrb <= "1111"; --S_AXI_WSTRB;
	wlast <= AXI_WLAST;
	awvalid <= AXI_AWVALID;
	wvalid <= AXI_WVALID;
	bready <= AXI_BREADY;
	awlen <= AXI_AWLEN;
	bresp <= "00"; -- OKAY response, always a success; see page 54 of axi-4 protocol specification document
	bid <= AXI_AWID; -- do not support multiple transactions.
	
	-- STATE MACHINE for reading instructions
	WR_STATE_REG: process (aclk)
	begin 
		if aclk'event and aclk = '1' then
			if aresetn = '0' then
				wr_currstate <= write_0 ;
			else
				wr_currstate <= wr_nextstate ;
			end if;
		end if ;
	end process WR_STATE_REG;
    
	wr_state_comb: process (wr_currstate, wvalid, wlast, FIFO_FULL) --, awvalid
	begin  --  process
		wr_nextstate <= wr_currstate ;  
		-- by default, does not change the state.
	
		awready <= '0';	 
		wready <= '0';
		bvalid <= '0'; -- always valid the response
		wr_file_reg <= '0';
		
		case wr_currstate is
	 
		-- WRITE DATA TO INTERNAL MEMORY --------------------------------------------------
			when write_0 =>
--				if awvalid = '1' then 
--					wr_nextstate <= write_1; 
--				end if;
--				awready <= '1';
				if wlast = '1' then 
					if wvalid = '1' then
						wr_nextstate <= write_1;
					end if;
				end if;
				awready <= '1';	 
				wready <= not (FIFO_FULL);
				wr_file_reg <= wvalid and (not (FIFO_FULL));
			when write_1 =>
--				if wlast = '1' then 
--					if wvalid = '1' then
--						wr_nextstate <= write_0;
--					end if;
--				end if;
--				wready <= '1';
--				wr_file_reg <= wvalid;
				wr_nextstate <= write_0;
				bvalid <= '1';
		end case;
	end process;
  
	-- Without State machine version -----------------------------------------------------------
--	awready <= '1';	 
--	wready <= not (FIFO_FULL);
--	bvalid <= '1'; -- always valid the response
--	wr_file_reg <= wvalid and (not (FIFO_FULL));
	
--	BVALID_REGISTER: process (aclk)
--   begin  
--		if aclk'event and aclk = '1' then
--			if aresetn = '0' then
--				bvalid <= '0'; 
--			elsif(wlast = '1') then
--				bvalid <= bready;
--			end if;
--		end if ;
--   end process BVALID_REGISTER;
	--------------------------------------------------------------------------------------------
	-- OUTPUTS
	--------------------------------------------------------------------------------------------
   AXI_WREADY <= wready;        
   AXI_BRESP <= bresp;         
   AXI_BVALID <= bvalid;        
   AXI_AWREADY <= awready;       
	AXI_BID <= bid;
	WE_MEM_SPACE <= wr_file_reg;
	DATAOUT <= wdata;

end Behavioral;

