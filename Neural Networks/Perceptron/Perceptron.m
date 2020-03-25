% Programado por: Uriel García Rivas
% Neural Networks
% Escuela Superior de Cómputo, Instituto Politécnico Nacional
disp('(1) Regla de Aprendizaje');
disp('(2) Método Gráfico');
load inputs.txt
load target.txt
w = -2 + (2+2)*rand(1,2);
resultadosW = [0,0;0,0];
resultadosW(2,1) = w(1);
resultadosW(2,2) = w(2);
bias = -1 + (1+1)*rand();
contador = 0;
tam = size(inputs);
switch input('Elija una opción:')
    case 1
        for j=1:5
            contador = 0;
            for i=1:tam(1)
                a = hardlim((w * inputs(i,:).')+bias);
                bias = bias + (target(i)-a);
                w = w + (target(i)-a)*(inputs(i,:));
                if(a == target(i))
                    contador = contador + 1;
                else
                    resultadosW(i+2,1) = w(1);
                    resultadosW(i+2,2) = w(2);
                end
            end
            if(contador == tam(1))
                disp('OMG');
                break;
            end
        end
        if(contador == tam(1))
            figure(1)
             xlim = get(gca,'XLim');
             ylim = get(gca,'YLim');
             plot( [(2*-max(inputs(:,1))), 2*max(inputs(:,1))], [0, 0], 'black');
             hold on
             plot( [0, 0], [(2*-max(inputs(:,2))), 2*max(inputs(:,2))], 'black');
             hold on
             plot([0, w(1)], [0, w(2)], 'red');
             plot([(-3*w(2))-bias, (3*w(2))-bias], [3*w(1), (-3)*w(1)], 'blue');
             hold on
             title('Gráfica')
             xlabel('Eje X')
             ylabel('Eje Y')
            for i=1:tam(1)
                x = inputs(i,1);
                y = inputs(i,2);
                if(target(i) == 1)
                    plot(x,y,'-go', 'MarkerFaceColor', 'g');
                else
                    plot(x,y,'-go');
                end
                hold on
            end
             grid
            hold off
            figure(2)
             plot(resultadosW, '-s');
             title('Gráfica de evolución de pesos')
             xlabel('Número de cambios en W')
             ylabel('w')
            grid
        else
            disp('Aprendizaje no exitoso, inténtelo de nuevo :(');
        end
        
    case 2
        for j=1:10
            contador = 0;
            w = -2 + (2+2)*rand(1,2);
            bias = rand();
            for i=1:tam(1)
                a = hardlim((w * inputs(i,:).')+bias);
                if(a == target(i))
                    contador = contador + 1;
                end
            end
            if(contador == tam(1))
                disp('Método Gráfico exitoso!');
                break;
            end
        end
        if(contador ~= tam(1))
            disp('Método Gráfico no exitoso, inténtelo de nuevo :(');
        end
        if(contador == tam(1))
            figure(1)
             A = [-bias w(2)-bias];
             B = [0 -w(1)];
             xlim = get(gca,'XLim');
             ylim = get(gca,'YLim');
             plot( [(2*-max(inputs(:,1))), 2*max(inputs(:,1))], [0, 0], 'black');
             hold on
             plot( [0, 0], [(2*-max(inputs(:,2))), 2*max(inputs(:,2))], 'black');
             hold on
             plot([0, w(1)], [0, w(2)], 'red');
             plot([(-3*w(2))-bias, (3*w(2))-bias], [3*w(1), (-3)*w(1)], 'blue');
             hold on
             title('Gráfica')
             xlabel('Eje X')
             ylabel('Eje Y')
            for i=1:tam(1)
                x = inputs(i,1);
                y = inputs(i,2);
                if(target(i) == 1)
                    plot(x,y,'-go', 'MarkerFaceColor', 'g');
                else
                    plot(x,y,'-go');
                end
                hold on
            end
             grid
            hold off
        end
    otherwise
        disp('Número introducido no corresponde, el programa cerrará.');
end

w2 = -1 + (1+1)*rand(2,2);
b = -1 + (1+1)*rand(1,2);
p = [1,1 ; 2,2 ; 2,-1 ; 2,0 ; -1,2 ; -2,1 ; -1,-1 ; -2,-2];
t = [0,0 ; 0,0 ; 0,1  ; 0,1 ;  1,0 ;  1,0 ;   1,1 ; 1,1];
tamanio = size(p);
rW1 = [0,0;0,0];
rW1(2,1) = w2(1);
rW1(2,2) = w2(2);
rW2 = [0,0;0,0];
rW2(2,1) = w2(1);
rW2(2,2) = w2(2);
contador1 = 0;
contador2 = 0;
a1 = [0;0];
for PORFAVORYA=1:3
    for k=1:tamanio(2)
        contador1 = 0;
        for j=1:3
            for i=1:tamanio(1)
                a1(k) = hardlim((w2(k,:) * p(i,:).')+b(k));
                b(k) = b(k) + (t(i,k)-a1(k));
                w2(k,:) = w2(k,:) + (t(i,k)-a1(k))*(p(i,:));
                if(a1(k) == t(i,k))
                    contador1 = contador1 + 1;
                else
                    if(k==1)
                        rW1(i+2,1) = w2(k,1);
                        rW1(i+2,2) = w2(k,2);
                    elseif (k==2)
                        rW2(i+2,1) = w2(k,1);
                        rW2(i+2,2) = w2(k,2);
                    end
                end
            end
            if(contador1 == tamanio(1))
                disp('OMG');
                contador2 = contador2+1;
                break;
            end
        end
    end
    if(contador2 == tamanio(2))
        break;
    end
end
if(contador2 == tamanio(2))
    figure(3)
     xlim = get(gca,'XLim');
     ylim = get(gca,'YLim');
     plot( [(2*-max(p(:,1))), 2*max(p(:,1))], [0, 0], 'black');
     hold on
     plot( [0, 0], [(2*-max(p(:,2))), 2*max(p(:,2))], 'black');
     hold on
     plot([0, w2(1,1)], [0, w2(1,2)], 'red');
     plot([(-3*w2(1,2))-b(1), (3*w2(1,2))-b(1)], [3*w2(1,1), (-3)*w2(1,1)], 'blue');
     
     plot([0, w2(2,1)], [0, w2(2,2)], 'red');
     plot([(-3*w2(2,2))-b(2), (3*w2(2,2))-b(2)], [3*w2(2,1), (-3)*w2(2,1)], 'blue');
     hold on
     title('Gráfica')
     xlabel('Eje X')
     ylabel('Eje Y')
    for i=1:tamanio(1)
        x = p(i,1);
        y = p(i,2);
        if(t(i,1) == 1)
            if (t(i,2) == 1)
                plot(x,y,'-mo', 'MarkerFaceColor', 'm');
            else
                plot(x,y,'-mo');
            end
        else
            if (t(i,2) == 1)
                plot(x,y,'-ms', 'MarkerFaceColor', 'm');
            else
                plot(x,y,'-ms');
            end
        end
        hold on
    end
     grid
    hold off
    figure(4)
     plot(rW1, '-s');
     plot(rW2, '-s');
     title('Gráfica de evolución de pesos')
     xlabel('Número de cambios en W')
     ylabel('w')
    grid
end