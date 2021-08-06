-- Pacote de operacoes em Ponto Fixo
----------------------------------------------------------------------------------------------------

PACKAGE fixed_package IS

    CONSTANT MAX_IND : INTEGER := 15;
    CONSTANT MIN_IND : INTEGER := -15;
    SUBTYPE fixed_range IS INTEGER RANGE MIN_IND TO MAX_IND;
    TYPE FIXED IS ARRAY (fixed_range RANGE <>) OF BIT;
	 
    FUNCTION to_fixed (arg_L: INTEGER; max_range: fixed_range := MAX_IND;
            min_range: fixed_range := 0) RETURN FIXED;
    FUNCTION to_integer (arg_L: fixed) RETURN INTEGER;
    FUNCTION "+"(arg_L, arg_R: fixed) RETURN FIXED;
    FUNCTION "+"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED;
    FUNCTION "+"(arg_L: fixed; arg_R: INTEGER) RETURN FIXED;
    FUNCTION "-"(arg_L, arg_R: fixed) RETURN FIXED;
    FUNCTION "*"(arg_L, arg_R: FIXED) RETURN FIXED;
    FUNCTION to_fixed (arg_L: real; max_range, min_range: fixed_range) RETURN FIXED;
    FUNCTION to_real (arg_L:FIXED) RETURN real;
    FUNCTION "+"(arg_L: FIXED; arg_R: REAL) RETURN FIXED;
    FUNCTION "+"(arg_L: REAL; arg_R: FIXED) RETURN FIXED;
    FUNCTION "-"(arg_L: FIXED; arg_R: REAL) RETURN FIXED;
    FUNCTION "-"(arg_L: REAL; arg_R: FIXED) RETURN FIXED;
    FUNCTION "*"(arg_L: FIXED; arg_R: REAL) RETURN FIXED;
    FUNCTION "*"(arg_L: REAL; arg_R: FIXED) RETURN FIXED;
    FUNCTION MAXIMUM(arg_L, arg_R: integer) RETURN INTEGER;
    FUNCTION MINIMUM(arg_L, arg_R: integer) RETURN INTEGER;
    FUNCTION COMP1_FIXED(arg_L: FIXED) RETURN FIXED;
    FUNCTION ADD_SUB_FIXED(arg_L, arg_R: FIXED; c: bit) RETURN FIXED;
    function MULT_FIXED (arg_L, arg_R: fixed) return fixed;

END fixed_package;

--Corpo do pacote
PACKAGE BODY fixed_package IS

	--DeclaraÃ§Ã£o das funcoes auxiliares do pacote fixed_package
--------------------------------------------------------------
	--Retorna o maior valor entre dois argumentos

	FUNCTION MAXIMUM(arg_L, arg_R: integer) RETURN INTEGER IS
		VARIABLE maior : INTEGER;
	BEGIN
		IF arg_L > arg_R THEN
			maior := arg_L;
		ELSE
			maior := arg_R;
		END IF;
		
		RETURN maior;
	END MAXIMUM;
--------------------------------------------------------------
	--Retorna o menor valor entre dois argumentos

	FUNCTION MINIMUM(arg_L, arg_R: integer) RETURN INTEGER IS
		VARIABLE menor : INTEGER;
	BEGIN
		IF arg_L < arg_R THEN
			menor := arg_L;
		ELSE
			menor := arg_R;
		END IF;
		
		RETURN menor;
	END MINIMUM;
--------------------------------------------------------------
	--Retorna o complemento de 1 do argumento

	FUNCTION COMP1_FIXED(arg_L: FIXED) RETURN FIXED IS
		VARIABLE cpl : FIXED(arg_L'LENGTH-1 DOWNTO 0);
	BEGIN
		FOR i IN 0 TO arg_L'LENGTH-1 LOOP
			cpl(i) := NOT arg_L(i);
		END LOOP;
	
		RETURN cpl;
	
	END COMP1_FIXED;
--------------------------------------------------------------
	--Retorna a soma de dois argumentos do tipo FIXED

	FUNCTION ADD_SUB_FIXED(arg_L, arg_R: FIXED; c: bit) RETURN FIXED IS
		VARIABLE v : bit := c;
		VARIABLE s : FIXED(arg_L'LENGTH-1 DOWNTO 0);
	BEGIN
		FOR i IN 0 TO arg_L'LENGTH-1 LOOP
	
			s(i) := arg_L(i) XOR arg_R(i) XOR v;
			v    := (arg_L(i) AND arg_R(i)) OR (v AND (arg_L(i) OR arg_R(i)));
		END LOOP;
		RETURN s;
	
	END ADD_SUB_FIXED;
--------------------------------------------------------------
	--Retorna o produto entre dois argumentos

	function MULT_FIXED (arg_L, arg_R: fixed) return fixed is
		--Os valores de m e n sao obtidos dos sinais a e b a serem multiplicados.
		constant m: integer := arg_L'length;		--Eh sempre igual a 32 para o projeto
		constant n: integer := arg_R'length;		--Eh sempre igual a 32 para o projeto
		alias arg_Lv : fixed(m-1 downto 0) is arg_L;
		alias arg_Rv : fixed(n-1 downto 0) is arg_R;
		
		variable RESULT: fixed(arg_L'RANGE);
			
		-- Variaveis para armazenamento dos dados de entrada e de saida no formato bit_vector
		variable a, b, p: bit_vector(m+n-1 downto 0);
	
		--Primeiro criamos um tipo ?matrix?
		type matrix is array (natural range <>, natural range <>) of bit;

		--Depois criamos as matrizes de interconexao:
		--Mi,j	Mij(i, j)		0 <= i <= m+n-1, 0 <= j <= m+n-1
		variable Mij: matrix(0 to m+n-1, 0 to m+n-1);
		--Ci,j	Cij(i, j)		0 <= i <= m+n-1, 0 <= j <= m+n
		variable Cij: matrix (m+n-1 downto 0, m+n downto 0);
		--Pi,j	Pij(i, j) 	0 <= i <= m+n, 0 <= j <= m+n
		variable Pij: matrix (m+n downto 0, m+n downto 0);

	begin
		--Transferencia dos valores de arg_Rv para Lv e Rv, respectivamente:
		transf_a: for i in arg_Lv'range loop
			a(i) := arg_Lv(i);
		end loop;
		transf_b: for i in arg_Rv'range loop
			b(i) := arg_Rv(i);
		end loop;

		--Extensao dos bits de sinal em a e b:
		a(m+n-1 downto m) := (others => arg_Lv(arg_Lv'left));	--(others => arg_Lv(arg_Lv'LEFT));		-- blinha(m+n-1 downto n => b('left)); para signed
		b(m+n-1 downto n) := (others => arg_Rv(arg_Rv'left));	--(others => arg_Rv(arg_Rv'LEFT));		-- alinha(m+n-1 downto m => a('left)); para signed

		--Inicializamos as matrizes de interconexao Cij e Pij:
		--Cij(i, 0) = 0
		initCij: for i in 0 to m+n-1 loop			-- for i in m+n-1 downto 0 loop para signed
			Cij(i, 0) := '0';
		end loop initCij;

		--Pij(i, 0) = 0
		initPij1: for i in 0 to m+n loop			-- for i in m+n downto 0 loop para signed / P(0,0) useless
			Pij(i, 0) := '0';
		end loop initPij1;

		--Pij(m, i) = 0
		initPij2: for i in 1 to m+n-1 loop
			Pij(m, i) := '0';
		end loop initPij2;

		--Para inicializar a matriz de interconexao Mij eh necessario utilizar lacos de iteracao:
		--Mij(i, j) = A(i)B(j)
		Mijrow: for j in 0 to m+n-1 loop
			Mijcol: for i in 0 to m+n-1 loop
				Mij(i,j) := a(i) and b(j);
			end loop Mijcol;
		end loop Mijrow;

		--Finalmente, podemos interligar os modulos de processamento.
		--Pi,j	Pij(i, j) 	0 <= i <= m, 0 <= j <= m+n
		Pijrow: for j in 0 to m+n-1 loop
			Pijcol: for i in 0 to m+n-1 loop
				Pij(i,j+1) := Pij(i+1,j) xor Mij(i,j) xor Cij(i,j);
				Cij(i,j+1) := (Pij(i+1,j) and (Mij(i,j) or Cij(i,j))) or (Mij(i,j) and Cij(i,j));
			end loop Pijcol;
		end loop Pijrow;

		--O resultado final p sera igual a:
		--p(i) = Pij(0, i+1)		0 <= i <= m+n-1
		initPi: for i in 0 to m+n-1 loop
			p(i) := Pij(0,i+1);
		end loop initPi;
			
		--RESULT(i) = p(0, i+1)		i in (p'range)
		genR: for i in RESULT'range loop
			RESULT(i) := p(i-2*RESULT'right);
		end loop genR;

		return RESULT;
	end MULT_FIXED;
--------------------------------------------------------------
	--Retorna a conversao de inteiro para ponto fixo

	FUNCTION to_fixed (arg_L: INTEGER; max_range: fixed_range := MAX_IND;
			min_range: fixed_range := 0) RETURN FIXED IS
		VARIABLE ponto_fixo : FIXED(max_range DOWNTO min_range);
		VARIABLE arg_L_v: INTEGER := arg_L;
	BEGIN
		
		-- Loop principal para preencher a parte inteira do ARRAY de ponto fixo
			
		a: FOR i IN max_range-1 DOWNTO 0 LOOP
			IF arg_L_v mod (2**i) > 0 OR arg_L_v = (2**i) THEN
				ponto_fixo(i) := '1';
				arg_L_v := arg_L_v - 2**i;
			ELSE
				ponto_fixo(i) := '0';
			END IF;
		END LOOP a;
				
		-- Condicao para preencher bit do signal do ARRAY
				
		IF arg_L < 0 THEN
			ponto_fixo(max_range) := '1';
		ELSE
			ponto_fixo(max_range) := '0';
		END IF;

		RETURN ponto_fixo;
	END to_fixed;
--------------------------------------------------------------
	--Retorna a conversao de ponto fixo para inteiro

	FUNCTION to_integer (arg_L: fixed) RETURN INTEGER IS
		VARIABLE inteiro : INTEGER;
	BEGIN
			
		-- Loop principal para somar os valores da parte inteira do ARRAY
			
		inteiro := 0;
		a: FOR i IN arg_L'HIGH-1 DOWNTO 0 LOOP
			IF arg_L(i) = '1' THEN
				inteiro := inteiro + 2**i;
			END IF;
		END LOOP a;

		-- Condicao para colocar o sinal do valor inteiro
		
		IF arg_L(arg_L'HIGH) = '1' THEN
			inteiro := (-1)*inteiro;
		END IF;
				
		RETURN inteiro;
	END to_integer;
--------------------------------------------------------------
	-- Retorna a soma de dois pontos fixos

	FUNCTION "+"(arg_L, arg_R: fixed) RETURN fixed IS
		VARIABLE soma : FIXED(MAX_IND DOWNTO MIN_IND);
	BEGIN
		soma := ADD_SUB_FIXED(arg_L, arg_R, '0');
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna a subtracao de dois pontos fixos

	FUNCTION "-"(arg_L, arg_R: fixed) RETURN fixed IS
		
		CONSTANT ind_soma_left : INTEGER := MAXIMUM(arg_L'LEFT,arg_R'LEFT); --O indice da esquerda na soma será o maior
		CONSTANT ind_soma_right : INTEGER := MAXIMUM(arg_L'RIGHT,arg_R'RIGHT);--O indice da direita na soma será o menor
		
		VARIABLE subtracao, complemento, um, maior : FIXED(ind_soma_left DOWNTO ind_soma_right);

	BEGIN -- Soma por complemento de 2
		IF arg_L'HIGH <= arg_R'HIGH THEN -- Busca o complemento de 2 do menor vetor
			maior := arg_R;
			complemento := COMP1_FIXED(arg_L);
		ELSE
			maior := arg_L;
			complemento := COMP1_FIXED(arg_R);
		END IF;
		um := complemento;
		um := (0 => '1', OTHERS => '0');
		complemento := ADD_SUB_FIXED(complemento, um, '0');
		subtracao := ADD_SUB_FIXED(maior, complemento, '0');
		RETURN subtracao;
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na soma de ponto fixo e inteiro

	FUNCTION "+"(arg_L: FIXED; arg_R: INTEGER) RETURN FIXED IS
	BEGIN
		RETURN arg_L + to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na soma de inteiro e ponto fixo
	
	FUNCTION "+"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED IS
	BEGIN
		RETURN to_fixed(arg_L, arg_R'HIGH, arg_R'LOW) + arg_R;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de ponto fixo e inteiro

	FUNCTION "-"(arg_L: FIXED; arg_R: INTEGER) RETURN FIXED IS
	BEGIN
		RETURN arg_L - to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de inteiro e ponto fixo
	
	FUNCTION "-"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED IS
	BEGIN
		RETURN to_fixed(arg_L, arg_R'HIGH, arg_R'LOW) - arg_R;
	END "-";
--------------------------------------------------------------
	-- Retorna a multiplicaÃ§Ã£o de dois pontos fixos

	FUNCTION "*"(arg_L, arg_R: FIXED) RETURN FIXED IS                        

		VARIABLE arg_L_ext : FIXED(2*arg_L'length-2 DOWNTO 0);   --Cria um vetor extendido     
		VARIABLE arg_R_ext : FIXED(2*arg_R'length-2 DOWNTO 0);   --Cria um vetor extendido 
		
		VARIABLE Resultado,Mult,Comp1,Comp2,Comp3 : FIXED(2*arg_L'length-2 DOWNTO 0);
		VARIABLE zero : FIXED(2*arg_L'length-2 DOWNTO 0);
		
	BEGIN
		arg_R_ext(arg_R'length-1 DOWNTO 0) := arg_R;                                   
		arg_R_ext(arg_R_ext'length-1 DOWNTO arg_R'length) := (OTHERS => arg_R(arg_R'high));    --O restante do vetor extendido copia o valor do MSB do vetor inicial
		arg_L_ext(arg_L'length-1 DOWNTO 0) := arg_L;
		arg_L_ext(arg_L_ext'length-1 DOWNTO arg_L'length) := (OTHERS => arg_L(arg_L'high));    --O restante do vetor extendido copia o valor do MSB do vetor inicial

		IF arg_L(arg_L'high)= '0' AND arg_R(arg_R'high) = '1' THEN   --argL < 0 e arg_R >0
		
			Comp1 := COMP1_FIXED(arg_R_ext);
			Comp2 := ADD_SUB_FIXED(zero, Comp1, '1');     --faz o complemento de 2
			Mult := MULT_FIXED(arg_L_ext,Comp2);
			Resultado := ADD_SUB_FIXED(zero, Mult, '1');	--faz o complemento de 2 da multiplicaÃ§Ã£o para obter o resultado final
		
		ELSIF arg_L(arg_L'high)= '1' AND arg_R(arg_R'high) = '0' THEN   --argL > 0 e arg_R < 0
		
			Comp1 := COMP1_FIXED(arg_L_ext);
			Comp2 := ADD_SUB_FIXED(zero, Comp1, '1');     --faz o complemento de 2
			Mult := MULT_FIXED(arg_R_ext,Comp2);
			Resultado := ADD_SUB_FIXED(zero, Mult, '1'); --faz o complemento de 2 da multiplicaÃ§Ã£o para obter o resultado final
		
		ELSIF arg_L(arg_L'high)= '1' AND arg_R(arg_R'high) = '1' THEN   --argL < 0 e arg_R < 0
		
			Comp1 := COMP1_FIXED(arg_L_ext);
			Comp2 := ADD_SUB_FIXED(zero, Comp1, '1');   --faz o complemento de 2 de arg_L
			Comp1 := COMP1_FIXED(arg_R_ext);
			Comp3 := ADD_SUB_FIXED(zero, Comp1, '1');   --faz o complemento de 2 de arg_R
			Resultado := MULT_FIXED(Comp3,Comp2);
			
		ELSE   --argL > 0 e arg_R > 0
			
			resultado := MULT_FIXED(arg_R_ext,arg_L_ext);
		
		END IF;

		RETURN resultado;
      END "*";
--------------------------------------------------------------
	-- Transforma real em fixed

	FUNCTION to_fixed (arg_L: real; max_range, min_range: fixed_range) RETURN FIXED IS
		CONSTANT RESULT0: FIXED(max_range downto min_range) := (others => '0');
		CONSTANT RESULTp1: FIXED(max_range downto min_range) := ("0111111111111111");
		CONSTANT RESULTm1: FIXED(max_range downto min_range) := ("1000000000000000");
		VARIABLE RESULT: FIXED(max_range downto min_range) := (others => '0');
		VARIABLE tmp1: INTEGER := 0;
	BEGIN
		IF abs(arg_L) < 2.0**(min_range) THEN
			--report "Too small." severity WARNING;
			RETURN RESULT0;
		END IF;
		IF abs(arg_L) >= 2.0**(max_range) THEN
			--report "Too big." severity WARNING;
			RETURN RESULTp1;
		END IF;
		IF abs(arg_L) <= -2.0**(max_range) THEN
			--report "Too bing." severity WARNING;
			RETURN RESULTm1;
		END IF;
		tmp1 := INTEGER(arg_L*2.0**(-min_range));
		RESULT := to_fixed(tmp1);
		RETURN RESULT;
	END FUNCTION;

--------------------------------------------------------------
	-- Transforma fixed em real
										      
	FUNCTION to_real (arg_L:FIXED) RETURN real IS

		VARIABLE arg_real : REAL := 0.0;

		VARIABLE arg_L_fim,Comp1 : FIXED(arg_L'RANGE);
		VARIABLE zero : FIXED(arg_L'LENGTH DOWNTO 0);

	BEGIN

		IF arg_L(arg_L'HIGH) = '1' THEN	--NÃºmero Ã© negativo

			Comp1 := COMP1_FIXED(arg_L);	--faz o complemento de 1
			arg_L_fim := ADD_SUB_FIXED(zero, Comp1, '1');	--faz o complemento de 2

			FOR i IN 0 TO (arg_L_fim'LENGTH-1) LOOP
			IF (arg_L(i) = '1') THEN
				arg_real := arg_real - (2.0**i);
			END IF;
			END LOOP;

		ELSE		--Sem bit de sinal, entao Ã© positivo

			FOR i IN 0 TO arg_L'LENGTH-1 LOOP
			IF (arg_L(i) = '1') THEN
				arg_real := arg_real + (2.0**i);
			END IF;
			END LOOP;

		END IF;

		RETURN arg_real;
	END to_real;						       
-------------------------------------------------------------- 
	-- Retorna ponto fixo na soma de ponto fixo e real

	FUNCTION "+"(arg_L: FIXED; arg_R: REAL) RETURN FIXED IS
	BEGIN
		RETURN arg_L + to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na soma de real e ponto fixo
	
	FUNCTION "+"(arg_L: REAL; arg_R: FIXED) RETURN FIXED IS
	BEGIN
		RETURN to_fixed(arg_L, arg_R'HIGH, arg_R'LOW) + arg_R;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de ponto fixo e real

	FUNCTION "-"(arg_L: FIXED; arg_R: REAL) RETURN FIXED IS
	BEGIN
		RETURN arg_L - to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de real e ponto fixo
	
	FUNCTION "-"(arg_L: REAL; arg_R: FIXED) RETURN FIXED IS
	BEGIN
		RETURN to_fixed(arg_L, arg_R'HIGH, arg_R'LOW) - arg_R;
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na multiplicacao de ponto fixo e real			   
	FUNCTION "*"(arg_L: fixed; arg_R: real) RETURN FIXED IS
	BEGIN
		RETURN MULT_FIXED(arg_L,to_fixed(arg_R, arg_L'HIGH, arg_L'LOW));
	END "*";
--------------------------------------------------------------
	-- Retorna ponto fixo na multiplicacao de real e ponto fixo
	FUNCTION "*"(arg_L: real; arg_R: fixed) RETURN FIXED IS
	BEGIN
		RETURN MULT_FIXED(to_fixed(arg_L, arg_R'HIGH, arg_R'LOW),arg_R);
	END "*";
--------------------------------------------------------------
END fixed_package;
