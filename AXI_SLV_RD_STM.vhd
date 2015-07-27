----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:51:04 02/13/2015 
-- Design Name: 
-- Module Name:    AXI_SLV_RD_STM - Behavioral 
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

entity AXI_SLV_RD_STM is
generic
  (
    AXI_DATA_WIDTH             : integer              := 32;
    AXI_ADDR_WIDTH             : integer              := 32;
    AXI_ID_WIDTH               : integer              := 4    
  );
port
(	
	-- FROM REGISTER BANK ---------------------
	DATAIN 							: in std_logic_vector(AXI_DATA_WIDTH-1 downto 0); --
	FIFO_VALID						: in std_logic;
	RE_MEM_SPACE					: out std_logic;
	-- AXI RELATED BELLOW ---------------------
	ACLK                     	: in  std_logic;
   ARESETN                  	: in  std_logic;
   AXI_ARADDR                 : in  std_logic_vector(AXI_ADDR_WIDTH-1 downto 0); --
   AXI_ARVALID                : in  std_logic; --
   AXI_RREADY                 : in  std_logic; --
   AXI_ARREADY                : out std_logic; --
   AXI_RDATA                  : out std_logic_vector(AXI_DATA_WIDTH-1 downto 0); --
   AXI_RRESP                  : out std_logic_vector(1 downto 0); --
   AXI_RVALID                 : out std_logic; --
   AXI_ARID                   : in  std_logic_vector(AXI_ID_WIDTH-1 downto 0); --
   AXI_ARLEN                  : in  std_logic_vector(7 downto 0); --
   AXI_ARSIZE                 : in  std_logic_vector(2 downto 0); --
   AXI_ARBURST                : in  std_logic_vector(1 downto 0); --
   AXI_ARCACHE                : in  std_logic_vector(3 downto 0); --
   AXI_ARPROT                 : in  std_logic_vector(2 downto 0); --
   AXI_RID                    : out std_logic_vector(AXI_ID_WIDTH-1 downto 0); -- 
   AXI_RLAST                  : out std_logic --
   -- AXI RELATED ABOVE  ---------------------
);
end AXI_SLV_RD_STM;

architecture Behavioral of AXI_SLV_RD_STM is
	
	type rd_fsm_states is (read_0, read_1); 
	signal rd_currstate, rd_nextstate: rd_fsm_states;
	
	signal araddr : std_Logic_vector(AXI_ADDR_WIDTH-1 downto 0);
	signal rdata : std_Logic_vector(AXI_DATA_WIDTH-1 downto 0);
	signal arvalid : std_logic;
	signal arready : std_logic;
	signal rready : std_logic;
	signal rvalid : std_logic;
	signal rlast : std_logic;
	signal arlen : std_logic_vector(7 downto 0);
	signal rresp : std_logic_vector(1 downto 0);
	signal rid : std_logic_vector(AXI_ID_WIDTH-1 downto 0);
	
	signal last_word, end_cnt, rd_en_burst_cnt : std_logic;
	signal rd_burst_cnt : std_logic_vector(7 downto 0);

begin

	--------------------------------------------------------------------------------------------
	-- INPUTS
	--------------------------------------------------------------------------------------------
   araddr <= AXI_ARADDR;
	rdata <= DATAIN;
	arvalid <= AXI_ARVALID;
	rready <= AXI_RREADY;
	arlen <= AXI_ARLEN;
	rresp <= "00"; -- OKAY response, always a success; see page 54 of axi-4 protocol specification document
	rid <= AXI_ARID; -- do not support multiple transactions.
	
	-- STATE MACHINE for reading instructions
	RD_STATE_REG: process (aclk)
	begin 
		if aclk'event and aclk = '1' then
			if aresetn = '0' then
				rd_currstate <= read_0 ;
			else
				rd_currstate <= rd_nextstate ;
			end if;
		end if ;
	end process RD_STATE_REG;
    
	rd_state_comb: process (rd_currstate, arvalid, rready, last_word, FIFO_VALID)
	begin  --  process
		rd_nextstate <= rd_currstate ;  
		-- by default, does not change the state.
	
		arready <= '0';	 
		rvalid <= '0';
		rlast <= '0';
		rd_en_burst_cnt <= '0';
		end_cnt <= '0';
		
		case rd_currstate is
	 
		-- READ DATA FROM EXTERNAL MEMORY --------------------------------------------------
			when read_0 =>
				if arvalid = '1' then 
					rd_nextstate <= read_1; -- READ DATA FROM EXTERNAL MEMORY
				end if;
				arready <= '1';
			when read_1 =>
				if last_word = '1' then 
					if rready = '1' then
						rd_nextstate <= read_0;
					end if;
				end if;
				rvalid <= FIFO_VALID; --'1';
				rlast <= last_word; 
				rd_en_burst_cnt <= rready and (not last_word);
				end_cnt <= rready and last_word; 
		end case;
	end process;
	
	RD_BURST_COUNTER: process (aclk)
   begin  
		if aclk'event and aclk = '1' then
			if aresetn = '0' then
				rd_burst_cnt <= (others => '0'); 
			else
				if end_cnt = '1' then 
					rd_burst_cnt <= (others => '0');
				elsif rd_en_burst_cnt = '1' then
					rd_burst_cnt <= rd_burst_cnt + 1; --instruction memory address is at bit level
				end if;
			end if;
		end if ;
   end process RD_BURST_COUNTER;
	
--	RD_VALID_REGISTER: process (aclk)
--   begin  
--		if aclk'event and aclk = '1' then
--			if aresetn = '0' then
--				rvalid <= '0'; 
--			else
--				rvalid <= FIFO_VALID and arvalid;
--			end if;
--		end if ;
--   end process RD_VALID_REGISTER;
	-- Without State Machine -------------------------------------------------------------------
	
--	arready <= '1';	 
--	rvalid <= FIFO_VALID; --'1'; -- fifo valid is the fifo ack to the read request
--	rlast <= last_word;
--	rd_en_burst_cnt <= rready and (not last_word);
--	end_cnt <= rready and last_word;
		
	--------------------------------------------------------------------------------------------
	
  -- PROBLEM - axi signals sent cannot depend on signals arrived
  last_word <= '1' when rd_burst_cnt = arlen else '0';	
  
--	readFromPE <= wready and axis_tvalid and (not last_word); --'1';
--	wr_en_burst_cnt <= wready and axis_tvalid and (not last_word);
--	end_cnt <= wready and last_word; 
--	wvalid <= axis_tvalid or last_word;

	--------------------------------------------------------------------------------------------
	-- OUTPUTS
	--------------------------------------------------------------------------------------------
    AXI_ARREADY <= arready;
    AXI_RDATA <= rdata;
    AXI_RRESP <= rresp;
    AXI_RVALID <= rvalid;
    AXI_RID <= rid;
    AXI_RLAST <= rlast;
	 RE_MEM_SPACE <= arvalid;
	 
end Behavioral;

