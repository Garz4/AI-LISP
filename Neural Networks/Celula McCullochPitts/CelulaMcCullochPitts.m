%% MIT License
%%
%% Copyright (c) 2023 Uriel Rivas
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% https://github.com/Garz4/artificial-intelligence/blob/main/LICENSE

% Programado por: Uriel Garcia Rivas
% Neural Networks
% Escuela Superior de Computo, Instituto Politecnico Nacional

x = input('Ingrese el numero de bits (2, 3, 4 o 5):');
targetNot = [0;1];
input1 = [1;0];
epoca = [1; -1; 2; -2; 3; -3; 4; -4; 5; -5];
for i = 1:size(epoca) %%Compuerta l�gica NOT (Estructura casi igual en su totalidad para las otras compuertas)
    verificador = 0; %%Este contador nos ayudar� a saber si una propuesta de soluci�n sirve para toda situaci�n
    theta = epoca(i);
    w1 = theta; %%Se inicializan los pesos sin�pticos
    for j = 1:size(input1)
        n = input1(j)*w1; %%Se determina n
        if n > theta %%Se realiza la funci�n f(n)
           a = 1;
        else
            a = 0;
        end
        if a == targetNot(j) %%Si en d sub�ndice i es correcto, aumenta el contador
            verificador = verificador + 1;
        end
    end
    if verificador == 2 %%Si el contador es igual al tama�o del target, el aprendizaje fue exitoso
        disp('Aprendizaje exitoso para compuerta NOT!');
        disp('w1 =');
        disp(w1);
        disp('theta =');
        disp(theta);
        break;
    end
    %%En caso contrario continua al siguiente ciclo y el contador reinicia
end
epoca = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14];
if x == 2 %%Se inicializan los valores de entrada y deseados de acuerdo al n�mero de bits prove�do
    targetAnd = [0;0;0;1];
    targetOr = [0;1;1;1];
    input1 = [0;1;0;1];
    input2 = [0;0;1;1];
    input3 = zeros(4);
    input4 = zeros(4);
    input5 = zeros(4);
end
if x == 3
    targetAnd = [0;0;0;0;0;0;0;1];
    targetOr = [0;1;1;1;1;1;1;1];
    input1 = [0;1;0;1;0;1;0;1];
    input2 = [0;0;1;1;0;0;1;1];
    input3 = [0;0;0;0;1;1;1;1];
    input4 = zeros(8);
    input5 = zeros(8);
end
if x == 4
    targetAnd = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1];
    targetOr = [0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];
    input1 = [0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1];
    input2 = [0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1];
    input3 = [0;0;0;0;1;1;1;1;0;0;0;0;1;1;1;1];
    input4 = [0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1];
    input5 = zeros(16);
end
if x == 5
    targetAnd = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1];
    targetOr = [0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];
    input1 = [0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1;0;1];
    input2 = [0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1;0;0;1;1];
    input3 = [0;0;0;0;1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;1;1;1;1;0;0;0;0;1;1;1;1];
    input4 = [0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1];
    input5 = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1;1];
end
for i = 1:size(epoca) %% Compuerta l�gica AND
    verificador = 0; 
    w1 = epoca(i); 
    w2 = w1;
    w3 = w1;
    w4 = w1;
    w5 = w1;
    if x == 2
        theta = w1;
    end
    if x == 3
        theta = w1+w2;
    end
    if x == 4
        theta = w1+w2+w3;
    end
    if x == 5
        theta = w1+w2+w3+w4;
    end
    for j = 1:size(input1)
        n = input1(j)*w1+input2(j)*w2+input3(j)*w3+input4(j)*w4+input5(j)*w5;
        if n > theta
            a = 1;
        else
            a = 0;
        end
        if a == targetAnd(j)
            verificador = verificador + 1;
        end
    end
    if x == 2
        if verificador == 4
            disp('Aprendizaje exitoso para compuerta AND de 2 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 3
        if verificador == 8
            disp('Aprendizaje exitoso para compuerta AND de 3 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 4
        if verificador == 16
            disp('Aprendizaje exitoso para compuerta AND de 4 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('w4 =');
            disp(w4);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 5
        if verificador == 32
            disp('Aprendizaje exitoso para compuerta AND de 5 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('w4 =');
            disp(w4);
            disp('w5 =');
            disp(w5);
            disp('theta =');
            disp(theta);
            break;
        end
    end
end
for i = 1:size(epoca) %% Compuerta l�gica OR
    verificador = 0;
    theta = epoca(end-i+1);
    w1 = epoca(i);
    w2 = w1;
    w3 = w1;
    w4 = w1;
    w5 = w1;
    for j = 1:size(input1)
        n = input1(j)*w1+input2(j)*w2+input3(j)*w3+input4(j)*w4+input5(j)*w5;
        if n > theta
            a = 1;
        else
            a = 0;
        end
        if a == targetOr(j)
            verificador = verificador + 1;
        end
    end
    if x == 2
        if verificador == 4
            disp('Aprendizaje exitoso para compuerta OR de 2 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 3
        if verificador == 8
            disp('Aprendizaje exitoso para compuerta OR de 3 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 4
        if verificador == 16
            disp('Aprendizaje exitoso para compuerta OR de 4 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('w4 =');
            disp(w4);
            disp('theta =');
            disp(theta);
            break;
        end
    end
    if x == 5
        if verificador == 32
            disp('Aprendizaje exitoso para compuerta OR de 5 bits!');
            disp('w1 =');
            disp(w1);
            disp('w2 =');
            disp(w2);
            disp('w3 =');
            disp(w3);
            disp('w4 =');
            disp(w4);
            disp('w5 =');
            disp(w5);
            disp('theta =');
            disp(theta);
            break;
        end
    end
end