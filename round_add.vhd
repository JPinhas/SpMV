--======================================================--
--                                                      --
--  NORTHEASTERN UNIVERSITY                             --
--  DEPARTMENT OF ELECTRICAL AND COMPUTER ENGINEERING   --
--  RAPID PROTOTYPING LABORATORY                        --
--                                                      --
--  AUTHOR       | Pavle Belanovic                      --
--  -------------+------------------------------------  --
--  DATE         | 20 June 2002                         --
--  -------------+------------------------------------  --
--  REVISED BY   | Haiqian Yu                           --
--  -------------+------------------------------------  --
--  DATE         | 18 Jan. 2003                         --
--  -------------+------------------------------------  --
--  REVISED BY   | Jainik Kathiara                      --
--  -------------+------------------------------------  --
--  DATE         | 21 Sept. 2010                        --
--======================================================--

--**********************************************************************************--
--                                                                                  --
--	Copyright (C) 2010		                                            --
--                                                                                  --
--	This program is free software; you can redistribute it and/or               --
--	modify it under the terms of the GNU General Public License                 --
--	as published by the Free Software Foundation; either version 2              --
--	of the License, or (at your option) any later version.                      --
--                                                                                  --
--	This program is distributed in the hope that it will be useful,             --
--	but WITHOUT ANY WARRANTY; without even the implied warranty of              --
--	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               --
--	GNU General Public License for more details.                                --
--                                                                                  --
--	You should have received a copy of the GNU General Public License           --
--	along with this program.  If not, see<http://www.gnu.org/licenses/>.        --
--                                                                           	    --
--**********************************************************************************--

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

----------------------------------------------------------
--                 Rounding addition                    --
----------------------------------------------------------
entity round_add is
	generic
	(
		exp_bits			:	integer	:= 8;
		man_bits_in		:	integer	:=	47;
		man_bits_out	:	integer	:=	23
	);
	port
	(
		--inputs
		CLK				:	in		std_logic;
		RESET				:	in		std_logic;
		CE					:	in		std_logic;
		STICKY_BIT_IN	:  in		std_logic;
		UNRND_RES_IN   :	in 	std_logic_vector(exp_bits+man_bits_in downto 0);
		READY				:	in		std_logic;
		EXCEPTION_IN	:	in		std_logic;
		--outputs
		DONE				:	out	std_logic;
		SIGN_OUT			:	out	std_logic;
		EXP_OUT			:	out	std_logic_vector(exp_bits-1 downto 0);
		MAN_OUT			:	out	std_logic_vector(man_bits_out-1 downto 0);
		EXCEPTION_OUT	:	out	std_logic
	);
end round_add;

----------------------------------------------------------
--                 Rounding addition                    --
----------------------------------------------------------
architecture round_add_arch of round_add is
	
	--SIGNALS
	signal	exp_int	:	std_logic_vector(exp_bits-1 downto 0)			:= (others=>'0');
	signal	man_int	:	std_logic_vector(man_bits_out-1 downto 0)		:=	(others=>'0');
	signal	exc_int	:	std_logic :=	'0';
	signal	exc_pa	:	std_logic :=	'0';
	signal	zeros		:	std_logic_vector(exp_bits+man_bits_out-1 downto 0)		:=	(others=>'0');
	
	-- Pipeline registers related
	signal man_int_reg :	std_logic_vector(man_bits_out-1 downto 0)		:=	(others=>'0');
	signal exc_reg : std_logic :=	'0';
	signal sign_reg : std_logic := '0';
	signal ready_reg : std_logic := '0';
	
	signal rounded_res, unrounded_res : std_logic_vector(exp_bits+man_bits_out-1 downto 0); 
	
	signal sign_in_synth : std_logic;
	signal ready_synth : std_logic;
	signal round_in_synth : std_logic;
	signal exception_in_synth : std_logic;
	signal exp_in_synth : std_logic_vector(exp_bits-1 downto 0);
	signal man_in_synth : std_logic_vector(man_bits_out-1 downto 0);
	
	signal round_result : std_logic;
	signal sticky_bit : std_logic;
	signal round_bit : std_logic;
	signal discarded_bits : std_logic_vector(man_bits_in-man_bits_out-1 downto 0);
	signal slice :	std_logic_vector(man_bits_out-1 downto 0);
	
	signal man : std_logic_vector(man_bits_in-1 downto 0);
	
begin

	--INPUT REGISTERS
	--SYNCHRONOUS
--	SYNTH_REG: process (CLK,RESET,STALL) is
--	begin
--		if(rising_edge(CLK)) then
--			if (RESET = '1') then
--				sign_in_synth <= '0';
--				ready_synth <= '0';
--				exception_in_synth <= '0';
--				round_in_synth <= '0';
--				exp_in_synth <= (others => '0');
--				man_in_synth <= (others => '0');
--			elsif (STALL = '0') then
--				sign_in_synth <= SIGN_IN;
--				ready_synth <= READY;
--				exception_in_synth <= EXCEPTION_IN;
--				round_in_synth <= ROUND_IN;
--				exp_in_synth <= EXP_IN;
--				man_in_synth <= MAN_IN;
--			end if;--CLK
--		end if;
--	end process SYNTH_REG;--main
				
	------------------------------------------------------------------------------------------------------------------------
	-- First Stage: DETERMINE IF RESULT NEEDS TO BE ROUNDED ----------------------------------------------------------------
	------------------------------------------------------------------------------------------------------------------------
	
	equal_width	:	if(man_bits_in = man_bits_out) generate
		
		-- remove the less significative bits which do not matter.
		man <= UNRND_RES_IN(man_bits_in-1 downto 0);
		
		slice <=	man(man_bits_in-1 downto man_bits_in-man_bits_out); 
		-- Determine the sticky bit by doing the OR of the shifted bits.
		sticky_bit <= STICKY_BIT_IN;
			
		round_bit <= sticky_bit; -- 24th bit
		
		-- Determine if the result have to be rounded or not
		-- first condition corresponds to discarded pare being more than lsb/2 
		-- and second condition enables untiing when discarded part equal do lsb/2
		round_result <= man(man_bits_in-man_bits_out) and round_bit;
		
	end generate;--0 bit more width
	
	------------------------------------------------------------------------------------------------------------------------
	
	one_bit_more_width	:	if(man_bits_in-1 = man_bits_out) generate
	
		-- DETERMINE IF RESULT NEEDS TO BE ROUNDED
		-- remove the less significative bits which do not matter.
		man <= UNRND_RES_IN(man_bits_in-1 downto 0);
		
		slice <=	man(man_bits_in-1 downto man_bits_in-man_bits_out); 
		-- Determine the sticky bit by doing the OR of the shifted bits.
		sticky_bit <= STICKY_BIT_IN;
			
		round_bit <= man(man_bits_in-man_bits_out-1); -- 24th bit
		
		-- Determine if the result have to be rounded or not
		-- first condition corresponds to discarded pare being more than lsb/2 
		-- and second condition enables untiing when discarded part equal do lsb/2
		round_result <= (round_bit and sticky_bit) or (slice(0) and round_bit);--(man(man_bits_in-man_bits_out-1) and round_bit);
	
	end generate;--1 bit more width
	
	------------------------------------------------------------------------------------------------------------------------
	
	twoORmore_bits_width	:	if(man_bits_in-1 > man_bits_out) generate
	
		-- DETERMINE IF RESULT NEEDS TO BE ROUNDED
		-- remove the less significative bits which do not matter.
		man <= UNRND_RES_IN(man_bits_in-1 downto 0);
		
		slice <=	man(man_bits_in-1 downto man_bits_in-man_bits_out); 
		-- Determine the sticky bit by doing the OR of the shifted bits.
		discarded_bits <= man(man_bits_in-man_bits_out-2 downto 0) & STICKY_BIT_IN;
		
		gate	:	parameterized_or_gate
			generic map
			(
				bits	=>	man_bits_in-man_bits_out -- all except 24 msb and the round bit. The previously sticky bit has to be counted.
			)
			port map
			(
				A	=>	discarded_bits, 
				O	=>	sticky_bit
			);
			
		round_bit <= man(man_bits_in-man_bits_out-1); -- 24th bit
		
		-- Determine if the result have to be rounded or not
		-- first condition corresponds to discarded pare being more than lsb/2 
		-- and second condition enables untiing when discarded part equal do lsb/2
		round_result <= (round_bit and sticky_bit) or (slice(0) and round_bit);--(man(man_bits_in-man_bits_out-1) and round_bit);
		
	end generate; --twoORmore_bits_width
	
	------------------------------------------------------------------------------------------------------------------------
	-- Second and Third Stage: Round result if the necessary conditions are met --------------------------------------------
	------------------------------------------------------------------------------------------------------------------------
	
	--ASYNCHRONOUS
	zeros <= (others=>'0');
		
	unrounded_res <= exp_in_synth & man_in_synth;--EXP_IN & MAN_IN;
	
	round_module	:	round_result_adder
		generic map
		(
			bits	=>	exp_bits+man_bits_out
		)
		port map
		(
			CLK 		=> CLK,
			STALL		=> '0', -- if increasing number of stages we have to take stall into account
			ADD_SUB	=> '1', -- always adding in rounding
			A			=>	unrounded_res,
			B			=>	zeros, 
			CIN		=>	round_in_synth, --ROUND_IN,
			S			=>	rounded_res,
			COUT		=>	exc_pa
		);	
	
	--ASSYNCHRONOUS
	exp_int <= rounded_res(exp_bits+man_bits_out-1 downto man_bits_out);
	man_int <= rounded_res(man_bits_out-1 downto 0);
	
	exc_int	<=	exc_pa OR exc_reg;
		
	--SYNCHRONOUS
	main: process (CLK,RESET,CE) is
	begin
		if(rising_edge(CLK)) then
			if (RESET = '1') then
--				sign_in_synth <= '0';
--				ready_synth <= '0';
--				exception_in_synth <= '0';
--				round_in_synth <= '0';
--				exp_in_synth <= (others => '0');
--				man_in_synth <= (others => '0');

--				exc_reg <= '0';
--				sign_reg <= '0';
--				ready_reg <= '0';
			
				SIGN_OUT			<= '0';
				EXP_OUT			<= (others=>'0');
				MAN_OUT			<=	(others=>'0');
				EXCEPTION_OUT	<=	'0';
				DONE				<=	'0';			
			elsif (CE = '1') then
--				sign_in_synth <= UNRND_RES_IN(UNRND_RES_IN'high);
--				ready_synth <= READY;
--				exception_in_synth <= EXCEPTION_IN;
--				round_in_synth <= round_result; --ROUND_IN;
--				exp_in_synth <= UNRND_RES_IN(man_bits_in+exp_bits-1 downto man_bits_in);
--				man_in_synth <= slice;
			
--				exc_reg <= exception_in_synth;--EXCEPTION_IN;
--				sign_reg <= sign_in_synth;--SIGN_IN;
--				ready_reg <= ready_synth;--READY;
			
				SIGN_OUT			<= sign_reg;
				EXP_OUT			<= exp_int;
				MAN_OUT			<=	man_int;
				EXCEPTION_OUT	<=	exc_int;
				DONE				<=	ready_reg;
			end if; --CLK
		end if;
	end process; --main
	
	sign_in_synth <= UNRND_RES_IN(UNRND_RES_IN'high);
	ready_synth <= READY;
	exception_in_synth <= EXCEPTION_IN;
	round_in_synth <= round_result; --ROUND_IN;
	exp_in_synth <= UNRND_RES_IN(man_bits_in+exp_bits-1 downto man_bits_in);
	man_in_synth <= slice;
				
	exc_reg <= exception_in_synth;--EXCEPTION_IN;
	sign_reg <= sign_in_synth;--SIGN_IN;
	ready_reg <= ready_synth;--READY;
	
end round_add_arch; -- end of architecture
