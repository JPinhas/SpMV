----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    00:28:46 02/13/2015 
-- Design Name: 
-- Module Name:    slave_interface - Behavioral 
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

entity slave_interface is
port
(		
	CLK				: in 	std_logic; --
	RST				: in 	std_logic; --
	PROC_WE 			: in 	std_logic; --
	CTRL_WE			: in  std_logic;
	CTRL_RE			: in  std_logic;
	ARADDR			: in 	std_logic_vector(AXI_SLV_AWIDTH-1 downto 0); --
	AWADDR			: in 	std_logic_vector(AXI_SLV_AWIDTH-1 downto 0); --
	PROC_WDATA		: in 	std_logic_vector(AXI_SLV_DWIDTH-1 downto 0); --
	NNZ_PKG2PE		: in 	std_logic_vector(AXI_SLV_DWIDTH-1 downto 0); --
	STM_STATUS		: in 	std_logic_vector(AXI_SLV_DWIDTH-1 downto 0); --
	CTRL_RDATA		: out slv_reg_type;
	PROC_RDATA		: out std_logic_vector(AXI_SLV_DWIDTH-1 downto 0)
);
end slave_interface;

architecture Behavioral of slave_interface is
	
	constant wr_proc_mem_space : integer := nr_proc_regs;
	constant wr_log2_proc_mem_space : integer := log2(wr_proc_mem_space);
	
	constant rd_proc_mem_space : integer := nr_axi_slv_regs;
	constant rd_log2_proc_mem_space : integer := log2(rd_proc_mem_space);
	
	signal sel_rd : std_logic_vector(rd_log2_proc_mem_space-1 downto 0);
	signal sel_wr : std_logic_vector(wr_log2_proc_mem_space-1 downto 0);
	signal we : std_logic_vector(wr_proc_mem_space-1 downto 0);
--	, re: std_logic_vector(NR_OF_MEM_SPACES-1 downto 0);
	signal full : std_logic_vector(wr_proc_mem_space-1 downto 0);
--	signal valid : std_logic_vector(NR_OF_MEM_SPACES-1 downto 0);
	signal empty : std_logic_vector(NR_OF_MEM_SPACES-1 downto 0);
	
	signal REG_DOUT : slv_reg_type;
	
--	signal zeros : std_logic_vector(number_of_regs-1 downto 0);
--	signal fifo_full_temp : std_logic;

	signal CTRL_WDATA : ctrl_reg_type;

begin

	sel_rd <= ARADDR(rd_log2_proc_mem_space+REF_ADDR_BIT-1 downto REF_ADDR_BIT);	
	sel_wr <= AWADDR(wr_log2_proc_mem_space+REF_ADDR_BIT-1 downto REF_ADDR_BIT);
	
	-- GENERIC DECODER
--	process(RD_IN, sel_rd)
--		variable temp : std_logic_vector(NR_OF_MEM_SPACES-1 downto 0);
--	begin
--		temp := (others => '0');
--		if (RD_IN = '1') then
--			temp(conv_integer(sel_rd)) := '1';
--		end if;
--		re <= temp;
--	end process;
	
	process(PROC_WE, sel_wr)
		variable temp : std_logic_vector(wr_proc_mem_space-1 downto 0);
	begin
		temp := (others => '0');
		if (PROC_WE = '1') then
			temp(conv_integer(sel_wr)) := '1';
		end if;
		we <= temp;
	end process;
	
	-----------------------------------------------------------------------------------------------
	-- REGISTERS
	-----------------------------------------------------------------------------------------------
	
	PROC_REGISTER_BANK : -- Processor written only
		for i in 0 to nr_proc_regs-1 generate   --D DSP is instantiated N_DSPS times.
		begin  --"begin" statement for "generate"
		
			RESULT_REGISTER_0 : process(CLK)
			begin
				if (rising_edge(CLK)) then
					if( RST = '1' ) then
						REG_DOUT(i) <= (others => '0'); -- because we do not want the controller to think number of columns is zero at the beginning
					else
						if (we(i) = '1') then -- and WR_IN = '1'
							REG_DOUT(i) <= PROC_WDATA; 
						end if;
					end if;
				end if;
			end process RESULT_REGISTER_0;
			
		end generate PROC_REGISTER_BANK;  --end "generate" block.

	CTRL_WDATA(0) <= NNZ_PKG2PE;
	CTRL_WDATA(1) <= STM_STATUS;
	
	CTRL_REGISTER_BANK : -- Controller written only
		for i in nr_proc_regs to nr_axi_slv_regs-1 generate   --D DSP is instantiated N_DSPS times.
		begin  --"begin" statement for "generate"
		
			RESULT_REGISTER_1 : process(CLK)
			begin
				if (rising_edge(CLK)) then
					if( RST = '1' ) then
						REG_DOUT(i) <= (others => '0');
					else
						if (CTRL_WE = '1') then 
							REG_DOUT(i) <= CTRL_WDATA(i-nr_proc_regs); 
						end if;
					end if;
				end if;
			end process RESULT_REGISTER_1;
			
		end generate CTRL_REGISTER_BANK;  --end "generate" block.

--	valid(number_of_regs-1 downto 0) <= (others => '0');
--	full <= fifo_full_temp & zeros(number_of_regs-1 downto 0);
--	DATAOUT <= REG_DOUT(conv_integer(sel_rd));
--	FIFO_VALID <= valid(conv_integer(sel_rd));

	full(nr_proc_regs-1 downto 0) <= (others => '0');
	
	CTRL_RDATA <= REG_DOUT;
	PROC_RDATA <= REG_DOUT(conv_integer(sel_rd));
	
end Behavioral;

