% Programado por: Uriel García Rivas
% Neural Networks
% Escuela Superior de Cómputo, Instituto Politécnico Nacional
numprot = input('Ingrese el numero de vectores prototipo: ');
numrasg = input('Ingrese el numero de rasgos por vector prototipo: ');
numinput = input('Ingrese el numero de entradas: ');
vectorW1 = zeros(numprot, numrasg);
vectorW2 = zeros(numprot, numprot);
vectorBIAS = zeros(numprot, 1);
vectorINPUT = zeros(numinput, numrasg);
vectorGRAFICA = zeros(1, numprot);
a1 = zeros(numprot, 1);
a2 = zeros(numprot, 2);
epsilon = 0;
for i=1:numprot
    disp('Prototipo ');
    disp(i);
    for j=1:numrasg
       disp('Rasgo ');
       disp(j);
       vectorW1(i, j) = input(' ');
    end
end
for i=1:numprot
    vectorBIAS(i, 1) = numrasg;
end
for i=1:numinput
    disp('Entrada ');
    disp(i);
    for j=1:numrasg
       disp('Rasgo ');
       disp(j);
       vectorINPUT(i, j) = input(' ');
    end
end
for i=1:numinput %% Comienza la red Hamming
    vectorGRAFICA = zeros(1, numprot);
    a2 = zeros(numprot, 2);
    entrada = vectorINPUT(i,:).';                  %% Basicamente, esto es
    a1 = purelin((vectorW1*entrada) + vectorBIAS); %% la capa feedforward
    epsilon = -1*((1/(numprot-1))/2); %% Se determina epsilon
    for j=1:numprot %% Se asignan valores para W2
        for k=1:numprot
            if j==k
                vectorW2(j,k) = 1;
            else
                vectorW2(j,k) = epsilon;
            end
        end
    end
    for t=0:20 %% Comienza la capa recurrente
        if t==0
            a2(:,1) = a1;
            vectorGRAFICA(2,:) = a1;
        else
            a2(:,1) = a2(:,2);
            vectorGRAFICA(t+2,:) = a2(:,1);
            vectorGRAFICA(t+3,:) = a2(:,2);
        end
        a2(:,2) = poslin(vectorW2*a2(:,1)); %% Función de la capa recurrente
        if a2(:,1) == a2(:,2) %% Verifica que los valores se repitan in order to converger
           disp('convergió');
           disp('i = ');
           disp(i);
           disp('t = ');
           disp(t);
           disp('a1 = ');
           disp(a1);
           disp('a2 = ');
           disp(a2);
           figure(i)
            plot(vectorGRAFICA);
            title('Entrada')
            xlabel('t')
            ylabel('a2')
           grid
           break; 
        end
    end
end