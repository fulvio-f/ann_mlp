--Alunos: Fulvio Favilla Filho		  - 11366893
--	  João Antônio Evangelista Garcia - 11234231
--        Guilherme Lopes Matias          - 11234102

--			  
-- Pacote de operações em Ponto Fixo
----------------------------------------------------------------------------------------------------

PACKAGE fixed_package IS

    CONSTANT MAX_IND : INTEGER := 15;
    CONSTANT MIN_IND : INTEGER := -15;
    SUBTYPE fixed_range IS integer RANGE MIN_IND TO MAX_IND;
    TYPE FIXED IS ARRAY (fixed_range RANGE <>) OF BIT;
    FUNCTION to_fixed (arg_L: INTEGER; max_range: fixed_range := MAX_IND;
            min_range: fixed_range := 0) RETURN FIXED;
    FUNCTION to_integer (arg_L: fixed) RETURN INTEGER;
    FUNCTION "+"(arg_L, arg_R: fixed) RETURN FIXED;
    FUNCTION "+"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED;
    FUNCTION "+"(arg_L: fixed; arg_R: INTEGER) RETURN INTEGER;
    FUNCTION "-"(arg_L, arg_R: fixed) RETURN FIXED;
    FUNCTION "*"(arg_L, arg_R: FIXED) RETURN FIXED;
    FUNCTION to_fixed(arg_L:REAL; max_range, min_range:fixed_range) RETURN FIXED;
    FUNCTION to_real (arg_L:FIXED) RETURN real;
    FUNCTION "+"(arg_L: FIXED; arg_R: REAL) RETURN FIXED;
    FUNCTION "+"(arg_L: REAL; arg_R: FIXED) RETURN FIXED;
    FUNCTION "-"(arg_L: FIXED; arg_R: REAL) RETURN FIXED;
    FUNCTION "-"(arg_L: REAL; arg_R: FIXED) RETURN FIXED;
    FUNCTION "*"(arg_L: FIXED, arg_R: REAL) RETURN FIXED;
    FUNCTION "*"(arg_L: REAL, arg_R: FIXED) RETURN FIXED;

END fixed_package;

--Corpo do pacote
PACKAGE BODY fixed_package IS

	--Declaração das funções auxiliares do pacote fixed_package
--------------------------------------------------------------
	--Retorna o maior valor entre dois argumentos

	FUNCTION MAX(arg_L, arg_R: integer) RETURN INTEGER IS
		VARIABLE maior : INTEGER;
	BEGIN
		IF arg_L > arg_R THEN
			maior := arg_L;
		ELSE
			maior := arg_R;
		END IF;
		
		RETURN maior;
	END MAX;
--------------------------------------------------------------
	--Retorna o menor valor entre dois argumentos

	FUNCTION MIN(arg_L, arg_R: integer) RETURN INTEGER IS
		VARIABLE menor : INTEGER;
	BEGIN
		IF arg_L < arg_R THEN
			menor := arg_L;
		ELSE
			menor := arg_R;
		END IF;
		
		RETURN menor;
	END MIN;
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

	FUNCTION MULT_FIXED(arg_L, arg_R: FIXED) RETURN FIXED IS
	
		--Dimensões das matrizes
		CONSTANT m : INTEGER := arg_L'LENGTH;
		CONSTANT n : INTEGER := arg_R'LENGTH;
		
		--Definindo um tipo matriz
		TYPE matrix IS ARRAY (NATURAL RANGE <>, NATURAL RANGE <>) OF BIT;
	
		--Matrizes utilizadas no cálculo
		VARIABLE Mij : matrix(0 TO m-1, 0 TO m+n-1);
		VARIABLE Cij : matrix(0 TO m-1, 0 TO m+n);
		VARIABLE Pij : matrix(0 TO m, 0 TO m+n);
	
		VARIABLE arg_R_linha : FIXED(m+n-1 DOWNTO 0);
		VARIABLE P : FIXED(m+n-1 DOWNTO 0);		--Produto final
	
	BEGIN
		
		arg_R_linha := (m+n-1 DOWNTO n => '0') & arg_R;
		
		
		INIT_Cij: FOR i IN 0 TO m-1 LOOP	  --Cij(i,0) = 0
			Cij(i,0) := '0';
		END LOOP INIT_Cij;
		
		
		INIT_Pij_coluna: FOR i IN 0 TO m LOOP    --Pij(i,0) = 0
			Pij(i,0) := '0';
		END LOOP INIT_Pij_coluna;
	
	
		INIT_Pij_linha: FOR j IN 1 TO m+n-1 LOOP    --Pij(m,j) = 0
			Pij(m,j) := '0';
		END LOOP INIT_Pij_linha;
	
	
		Mij_coluna: FOR i IN m-1 DOWNTO 0 LOOP
		
			Mij_linha: FOR j IN m+n-1 DOWNTO 0 LOOP
			
				Mij(i,j) := arg_L(i) AND arg_R_linha(j);
				
			END LOOP Mij_linha;
			
		END LOOP Mij_coluna;
			
			
		Pij_coluna: FOR i IN m-1 DOWNTO 0 LOOP
		
			Pij_linha: FOR j IN m+n-1 DOWNTO 0 LOOP
			
				Pij(i,j+1) := Pij(i+1,j) XOR Mij(i,j) XOR Cij(i,j);
				
				Cij(i,j+1) := (Pij(i+1,j) AND (Mij(i,j) OR Cij(i,j))) OR (Mij(i,j) AND Cij(i,j));
				
			END LOOP Pij_linha;
			
		END LOOP Pij_coluna;
		
	
	
		P_final: FOR i IN m+n-1 DOWNTO 0 LOOP		--Obtendo os valores para o produto P
			P(i) := Pij(0,i+1);
		END LOOP P_final;
	
	
		RETURN P;
	
	END MULT_FIXED;
--------------------------------------------------------------
	--Retorna a conversao de inteiro para ponto fixo

	FUNCTION to_fixed (arg_L: INTEGER; max_range: fixed_range := MAX_IND;
			min_range: fixed_range := 0) RETURN FIXED IS
		VARIABLE ponto_fixo : FIXED;
	BEGIN
		
		-- Loop principal para preencher a parte inteira do ARRAY de ponto fixo
			
		a: FOR i IN max_range-1 DOWNTO 0 LOOP
			IF arg_L mod (2**i) > 0 OR arg_L = (2**i) THEN
				ponto_fixo(i) := '1';
				arg_L := arg_L - 2**i;
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
			IF fixed(i) = '1' THEN
				inteiro := inteiro + 2**i;
			END IF;
		END LOOP a;

		-- Condicao para colocar o sinal do valor inteiro
		
		IF ponto_fixo(arg_L'HIGH) = '1' THEN
			inteiro := (-1)*inteiro;
		END IF;
				
		RETURN inteiro;
	END to_integer;
--------------------------------------------------------------
	-- Retorna a soma de dois pontos fixos

	FUNCTION "+"(arg_L, arg_R: fixed) RETURN fixed IS
		VARIABLE soma : FIXED;
	BEGIN
		soma := ADD_SUB_FIXED(arg_L, arg_R, '0');
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna a subtracao de dois pontos fixos

	FUNCTION "-"(arg_L, arg_R: fixed) RETURN fixed IS
		VARIABLE subtracao, complemento, um, maior : FIXED;
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
		VARIABLE soma,arg_R_fixed : FIXED;
	BEGIN
		arg_R_fixed := to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
		soma := arg_L + arg_R_fixed;
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na soma de inteiro e ponto fixo
	
	FUNCTION "+"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED IS
		VARIABLE soma,arg_L_fixed : FIXED;
		VARIABLE inteiro : INTEGER;
	BEGIN
		arg_L_fixed := to_fixed(arg_L, arg_R'HIGH, arg_R'LOW);
		soma := arg_L_fixed + arg_R;
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de ponto fixo e inteiro

	FUNCTION "-"(arg_L: FIXED; arg_R: INTEGER) RETURN FIXED IS
		VARIABLE subtracao,arg_R_fixed : FIXED;
	BEGIN
		arg_R_fixed := to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
		subtracao := arg_L - arg_R_fixed;
		RETURN subtracao;
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de inteiro e ponto fixo
	
	FUNCTION "-"(arg_L: INTEGER; arg_R: FIXED) RETURN FIXED IS
		VARIABLE subtracao,arg_L_fixed : FIXED;
	BEGIN
		arg_L_fixed := to_fixed(arg_L, arg_R'HIGH, arg_R'LOW);
		subtracao := arg_L_fixed - arg_R;
		RETURN subtracao;
	END "-";
--------------------------------------------------------------
	-- Retorna a multiplicação de dois pontos fixos

	FUNCTION "*"(arg_L, arg_R: FIXED) RETURN FIXED IS                        

		VARIABLE arg_L_ext : FIXED (2*arg_L'length-2 DOWNTO 0);   --Cria um vetor extendido     
		VARIABLE arg_R_ext : FIXED (2*arg_R'length-2 DOWNTO 0);   --Cria um vetor extendido 
		
		VARIABLE Resultado,Mult,Comp1,Comp2,Comp3 : FIXED;
			
	BEGIN
		arg_R_ext(arg_R'length-1 DOWNTO 0) := arg_R;                                   
		arg_R_ext(arg_R_ext'length-1 DOWNTO arg_R'length) := (OTHERS => arg_R(arg_R'high));    --O restante do vetor extendido copia o valor do MSB do vetor inicial
		arg_L_ext(arg_L'length-1 DOWNTO 0) := arg_L;
		arg_L_ext(arg_L_ext'length-1 DOWNTO arg_L'length) := (OTHERS => arg_L(arg_L'high));    --O restante do vetor extendido copia o valor do MSB do vetor inicial

		IF arg_L(arg_L'high)= '0' AND arg_R(arg_R'high) = '1' THEN   --argL < 0 e arg_R >0
		
			Comp1 := COMP1_FIXED(arg_R_ext);
			Comp2 := ADD_SUB('0', Comp1, 1);     --faz o complemento de 2
			Mult := MULT_FIXED(arg_L_ext,Comp2);
			Resultado = ADD_SUB('0', Mult, 1);	--faz o complemento de 2 da multiplicação para obter o resultado final
		
		ELSIF arg_L(arg_L'high)= '1' AND arg_R(arg_R'high) = '0' THEN   --argL > 0 e arg_R < 0
		
			Comp1 := COMP1_FIXED(arg_L_ext);
			Comp2 := ADD_SUB('0', Comp1, 1);     --faz o complemento de 2
			Mult := MULT_FIXED(arg_R_ext,Comp2);
			Resultado := ADD_SUB('0', Mult, 1); --faz o complemento de 2 da multiplicação para obter o resultado final
		
		ELSIF arg_L(arg_L'high)= '1' AND arg_R(arg_R'high) = '1' THEN   --argL < 0 e arg_R < 0
		
			Comp1 := COMP1_FIXED(arg_L_ext);
			Comp2 := ADD_SUB('0', Comp1, 1);   --faz o complemento de 2 de arg_L
			Comp1 := COMP1_FIXED(arg_R_ext);
			Comp3 := ADD_SUB('0', Comp1, 1);   --faz o complemento de 2 de arg_R
			Resultado := MULT_FIXED(Comp3,Comp2);
			
		ELSE   --argL > 0 e arg_R > 0
			
			resultado := MULT_FIXED(arg_R_ext,arg_L_ext);
		
		END IF;

		RETURN resultado;
      END "*";
--------------------------------------------------------------
	-- Transforma real em fixed

	FUNCTION to_fixed(arg_L:REAL;max_range,min_range:fixed_range) RETURN FIXED IS

		VARIABLE arg_fixed : FIXED;
		VARIABLE arg_int : REAL;
		VARIABLE arg_dec : REAL;

	BEGIN

		IF abs(arg_L) < 2**(min_range) THEN
			arg_fixed := OTHERS => '0';

		ELSIF arg_L >= 2**(max_range) THEN;
			arg_fixed := (max_range-1)=>('0', OTHERS => '1');

		ELSIF arg_L <= -2**(max_range) THEN;
			arg_fixed := (max_range-1)=>('1', OTHERS => '0');

		ELSIF 2**min_range <= abs(arg_L) <2**(max_range) THEN

			arg_int := arg_L mod 1;
			arg_dec := arg_L - arg_int;

			inteiro:FOR i IN max_range'left-1 DOWNTO 0 LOOP
				IF arg_int mod (2.0**i) >= 0 THEN
					arg_fixed(i) := '1';
					arg_int := arg_int - 2.0**i;
				ELSE
					arg_fixed(i) := '0';
				END IF;
			END LOOP inteiro;


			decimal:FOR i IN 0 TO min_range'right LOOP
				IF arg_dec mod (2.0**i) >= 0 AND arg_dec > 0.00001 THEN
					arg_fixed(i) := '1';
					arg_dec := arg_dec - 2**i;
				ELSE
					arg_fixed(i) := '0';
				END IF;
			END LOOP decimal;
		END IF;

		IF arg_L > 0 THEN
			arg_fixed(max_range) := 0

		ELSE
			arg_fixed(max_range) := 1
		END IF;

		RETURN arg_fixed
	END to_fixed;

--------------------------------------------------------------
	-- Transforma fixed em real
										      
	FUNCTION to_real (arg_L:FIXED) RETURN real IS

		VARIABLE arg_real : REAL := 0;

		VARIABLE arg_L_fim : FIXED;

	BEGIN

		IF arg_L'HIGH = '1' THEN	--Número é negativo

			Comp1 := COMP1_FIXED(arg_L);	--faz o complemento de 1
			arg_L_fim := ADD_SUB('0', Comp1, 1);	--faz o complemento de 2

			FOR i IN arg_L_fim'RANGE-1 LOOP
			IF (arg_L(i) = '1') THEN
				arg_real := arg_real - (2.0**i);
			END IF;
			END LOOP;

		ELSE		--Sem bit de sinal, entao é positivo

			FOR i IN arg_L'RANGE-1 LOOP
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
		VARIABLE soma, arg_R_fixed : FIXED;
	BEGIN
		arg_R_fixed := to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
		soma := arg_L + arg_R_fixed;
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na soma de real e ponto fixo
	
	FUNCTION "+"(arg_L: REAL; arg_R: FIXED) RETURN FIXED IS
		VARIABLE soma, arg_L_fixed : FIXED;
	BEGIN
		arg_L_fixed := to_fixed(arg_L, arg_R'HIGH, arg_R'LOW);
		soma := arg_L_fixed + arg_R;
		RETURN soma;
	END "+";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de ponto fixo e real

	FUNCTION "-"(arg_L: FIXED; arg_R: REAL) RETURN FIXED IS
		VARIABLE subtracao, arg_R_fixed : FIXED;
	BEGIN
		arg_R_fixed := to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
		subtracao := arg_L - arg_R_fixed;
		RETURN subtracao;
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na subtracao de real e ponto fixo
	
	FUNCTION "-"(arg_L: REAL; arg_R: FIXED) RETURN FIXED IS
		VARIABLE subtracao, arg_L_fixed : FIXED;
	BEGIN
		arg_L_fixed := to_fixed(arg_L, arg_R'HIGH, arg_R'LOW);
		subtracao := arg_L_fixed - arg_R;
		RETURN subtracao;
	END "-";
--------------------------------------------------------------
	-- Retorna ponto fixo na multiplicacao de ponto fixo e real			   
	FUNCTION "*"(arg_L: fixed, arg_R: real) RETURN FIXED IS
		VARIABLE produto, arg_R_fixed : FIXED;
	BEGIN
		arg_R_fixed := to_fixed(arg_R, arg_L'HIGH, arg_L'LOW);
		produto := MULT_FIXED(arg_L,arg_R_fixed);
		RETURN produto;
	END "*";
--------------------------------------------------------------
	-- Retorna ponto fixo na multiplicacao de real e ponto fixo
	FUNCTION "*"(arg_L: real, arg_R: fixed) RETURN FIXED IS
		VARIABLE produto, arg_L_fixed : FIXED;
	BEGIN
		arg_L_fixed := to_fixed(arg_L, arg_R'HIGH, arg_R'LOW);
		produto := MULT_FIXED(arg_L_fixed,arg_R);
		RETURN produto;
	END "*";
--------------------------------------------------------------
END fixed_package;
