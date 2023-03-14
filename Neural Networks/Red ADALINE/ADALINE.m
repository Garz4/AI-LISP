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

% Programado por: Uriel Garc�a Rivas
% Neural Networks
% Escuela Superior de C�mputo, Instituto Polit�cnico Nacional

clc
disp('1.- Modo Clasificador');
disp('2.- Modo Regresor');
o = input('Introduzca la opci�n deseada: ');
if(o == 1)
    bias = -1 + (1+1)*rand(2,1);
    P = [0,0;0,1;0,1;1,1];
    t = [0,0;0,1;0,1;1,1];
elseif(o == 2)
    load P.txt
    load t.txt
end
epochmax = input('Introduzca el n�mero m�ximo de �pocas: ');
eepoch = input('Introduzca el objetivo a llegar en la se�al de error: ');
alpha = input('Introduzca el valor del factor de aprendizaje: ');
tam = size(P);
w = -1 + (1+1)*rand(2,tam(2));
totalerrores = zeros(2,1);
contadorpesos = 1;
tamtarget = size(t);
contadorgraficas = 1;
totalsalidas = zeros();
aprendizaje = zeros(tamtarget(1),tamtarget(2)+1);
for i=2:tamtarget(2)+1
    aprendizaje(:,i) = t(:,i-1);
end
for target=1:tamtarget(2)
    for j=1:tam(2)
        w(contadorpesos,j) = -1 + (1+1)*rand();
    end
    for j=1:epochmax
        Eepoch=0;
        for i=1:tam(1)
            if(o == 1)
               a = purelin( w(contadorpesos,:)*P(i,:).' + bias(contadorpesos) );
               totalsalidas(i) = a;
               bias(contadorpesos+1) = bias(contadorpesos) + (2*alpha*( (t(i)-a) ));
            elseif(o == 2)
                a = purelin( w(contadorpesos,:)*P(i,:).' );
                aprendizaje(i,1) = a;
            end
            totalerrores(contadorpesos) = (t(i,target)-a);
            Eepoch = Eepoch + totalerrores(contadorpesos);
            w(contadorpesos+1,:) = w(contadorpesos,:) + (2*alpha*( (t(i,target)-a) )*P(i,:));
            contadorpesos = contadorpesos + 1;
        end
        Eepoch = Eepoch/tam(1);
        if(Eepoch == 0)
            if(o == 2)
                figure(contadorgraficas)
                    plot(aprendizaje)
                    title('Gr�fica de aprendizaje')
                    xlabel('ti (segundos)')
                    ylabel('f(ti)')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            if(o == 1)
                figure(contadorgraficas)
                    plot(bias)
                    title('Gr�fica de evoluci�n de bias')
                    xlabel('N�mero de cambios en BIAS')
                    ylabel('BIAS')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            figure(contadorgraficas)
                plot(w)
                title('Gr�fica de evoluci�n de pesos')
                xlabel('N�mero de cambios en W')
                ylabel('w')
            grid
            disp('Aprendizaje exitoso! Epoca:');
            disp(j);
            contadorgraficas = contadorgraficas+1;
            break;
        elseif(Eepoch < eepoch)
            if(o == 2)
                figure(contadorgraficas)
                    plot(aprendizaje)
                    title('Gr�fica de aprendizaje')
                    xlabel('ti (segundos)')
                    ylabel('f(ti)')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            if(o == 1)
                figure(contadorgraficas)
                    plot(bias)
                    title('Gr�fica de evoluci�n de bias')
                    xlabel('N�mero de cambios en BIAS')
                    ylabel('BIAS')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            disp('Aprendizaje exitoso! Epoca:');
            figure(contadorgraficas)
                plot(w)
                title('Gr�fica de evoluci�n de pesos')
                xlabel('N�mero de cambios en W')
                ylabel('w')
            grid
            disp(j);
            contadorgraficas = contadorgraficas+1;
            break;
        elseif(j == epochmax)
            if(o == 2)
                figure(contadorgraficas)
                    plot(aprendizaje)
                    title('Gr�fica de aprendizaje')
                    xlabel('ti (segundos)')
                    ylabel('f(ti)')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            if(o == 1)
                figure(contadorgraficas)
                    plot(bias)
                    title('Gr�fica de evoluci�n de bias')
                    xlabel('N�mero de cambios en BIAS')
                    ylabel('BIAS')
                grid
                contadorgraficas = contadorgraficas+1;
            end
            figure(contadorgraficas)
                plot(w)
                title('Gr�fica de evoluci�n de pesos')
                xlabel('N�mero de cambios en W')
                ylabel('w')
            grid
            disp('Aprendizaje no exitoso! :( Epoca:');
            disp(j);
            contadorgraficas = contadorgraficas+1;
            break;
        end
    end
end