
                                    
                -- recebe os resultados dos processadores
                when get_results =>
					pkg_stm_code <= "1000";
                    -- PID
                    instruction(3*DATA_WIDTH+CODE_BITS downto 3*DATA_WIDTH) <= ('0' & counter_curr(CODE_BITS-1 downto 0))+1;
                    -- Indice
                    instruction(ceil_log2(SIZE)-1 downto 0) <= counter_curr(counter_curr'high-1 downto CODE_BITS);
					valid <= '1';   
					counter_next <= counter_curr + 1;
					next_state <= check_results;
                            
                when check_results =>
						pkg_stm_code <= "1001";
                    code(0) <= '0';
					if counter_curr = rows_total then
						counter_next <= (others => '0');
						next_state <= reset;
                    elsif data_empty = '0' and m_axis_tready = '1' and to_integer(unsigned(counter_curr)) < P*SIZE then
                         next_state <= get_results;
                    end if;