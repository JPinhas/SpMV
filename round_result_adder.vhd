----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:30:59 11/05/2013 
-- Design Name: 
-- Module Name:    round_result_adder - Behavioral 
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
--======================================================--
--                      LIBRARIES                       --
--======================================================--

-- IEEE Libraries --
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

-- float
library mylibs_v1_00_a;
use mylibs_v1_00_a.float_pkg.all;

Library UNISIM;
use UNISIM.vcomponents.all;

Library UNIMACRO;
use UNIMACRO.vcomponents.all;
----------------------------------------------------------
--                Parameterized adder                   --
----------------------------------------------------------
entity round_result_adder is
	generic
	(
		bits			:	integer		:=	31
	);
	port
	(
		--inputs
		CLK 		: in std_logic;
      STALL		: in std_logic;
		ADD_SUB	: in std_logic;
		A			:	in	std_logic_vector(bits-1 downto 0);
		B			:	in	std_logic_vector(bits-1 downto 0);
		CIN			:	in	std_logic;
		--outputs
		S			:	out	std_logic_vector(bits-1 downto 0)	:=	(others=>'0');
		COUT			:	out	std_logic	:=	'0'
	);
end round_result_adder;

----------------------------------------------------------
--           Haiqian's parameterized_adder              --
--           Using operators                            --
----------------------------------------------------------
architecture adder_arch of round_result_adder is

--	--SIGNALS

--	signal not_stall : std_logic;

	signal	A_ext	:	std_logic_vector(bits downto 0);
	signal	B_ext	:	std_logic_vector(bits downto 0);
	
	signal add: std_logic_vector (bits downto 0) :=	(others=>'0');
	  
	type pipe_regs_type is array (0 downto 0) of std_logic_vector (bits downto 0);
	signal pipe_regs: pipe_regs_type;
		
--	attribute addstyle : string;
--	attribute addstyle of add : signal is "lut";
	
begin

--	not_stall <= not STALL;
	
	-- ADDSUB_MACRO: Variable width & latency - Adder / Subtractor implemented in a DSP48E
   --               Virtex-7
   -- Xilinx HDL Language Template, version 14.5

--   ADDSUB_MACRO_inst : ADDSUB_MACRO
--   generic map (
--      DEVICE => "7SERIES", -- Target Device: "VIRTEX5", "7SERIES", "SPARTAN6" 
--      LATENCY => 1,        -- Desired clock cycle latency, 0-2
--      WIDTH => bits)         -- Input / Output bus width, 1-48
--   port map (
--      CARRYOUT => COUT, -- 1-bit carry-out output signal
--      RESULT => S,     -- Add/sub result output, width defined by WIDTH generic
--      A => A,               -- Input A bus, width defined by WIDTH generic
--      ADD_SUB => ADD_SUB,   -- 1-bit add/sub input, high selects add, low selects subtract
--      B => B,               -- Input B bus, width defined by WIDTH generic
--      CARRYIN => CIN,   -- 1-bit carry-in input
--      CE => not_stall,      -- 1-bit clock enable input
--      CLK => CLK,           -- 1-bit clock input
--      RST => '0'            -- 1-bit active high synchronous reset
--   );
   -- End of ADDSUB_MACRO_inst instantiation
	
	A_ext(bits)	<=	'0';
	B_ext(bits)	<=	'0';
	A_ext(bits-1 downto 0)	<=	A;
	B_ext(bits-1 downto 0)	<=	B;
	
	add <= A_ext +	B_ext	+ CIN;
	  
--	p_regs12: process (CLK, STALL)
--	begin  -- process l_regs
--	if (rising_edge(CLK)) then
--		if STALL = '0' then
----			pipe_regs(ADD_MANTISSA_DELAY-1 downto 1) <= pipe_regs(ADD_MANTISSA_DELAY-2 downto 0);
--			pipe_regs(0) <= add;
--		end if;
--	end if;
--	end process p_regs12;
	  
	pipe_regs(0) <= add;
	
	S <= pipe_regs(0)(bits-1 downto 0);
	COUT <= pipe_regs(0)(bits);
	
end adder_arch; -- end of architecture
