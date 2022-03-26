clear all;
%% 1.1 Условие и оценка СКО в зависимости от NP и R
%Путем имитационного моделирования канала передачи информации
%с полезным импульсным радиосигналом и частотной модуляцией (несущая
%частота – 300 Гц) в среде с мешающим аддитивным белым шумом, построить
%зависимость вероятности уверенного приема сигнала PO от мощности шума

%Задание интервала дискретизации по времени и количества отсчетов
%на интервале моделирования [0,10] с.
Ts=0.001;
Ns=10000;
% 1. Оценка PO в зависимости от NP и R при фиксированных mr и Am
% Задание неварьируемых величин
Am = 50;
mr = 1;
Hz = 1;
Fr = 300;
nf = 2;
% format: NP R
minf = [0.04 3];
maxf = [0.9 8];
%формирование дробного двухуровневого плана эксперимента
%для учета взаимодействий
fracfact('a b ab' );
N=2^nf;
fracplan=ans;
fictfact=ones(N,1);
X=[fictfact ans]';
fraceks=zeros(N,nf);
for i = 1:nf
  for j = 1:N
    fraceks(j,i)=minf(i)+(fracplan(j,i)+1)*(maxf(i)-minf(i))/2;
  end
end
fraceks
%тактическое планирование эксперимента
%задание доверительного интервала и уровня значимости
dp=0.05;
alpha=0.07;
%определение t-критического
tkr_alpha=norminv(1-alpha/2);
%определение требуемого числа испытаний
NE = round(tkr_alpha^2/(4*dp^2));
%% 1.2 Планирование
%цикл по совокупности экспериментов стратегического плана
%цикл по совокупности экспериментов стратегического плана
counter = 0;
for j=1:N
 a=fraceks(j,1);
 b=fraceks(j,2);
 NP = a;
 R = b;
 u = [];
 %цикл статистических испытаний с фиксированным объемом
 %выборки для достижения заданной точности оценки показателя

 for k=1:NE
  %имитация функционирования системы
  to = round(rand * 100); %инициализация генератора шума
  sim('trenl', Ts*Ns);
  u0 = simout;
  u1 = simout1;
  %u1 = squeeze(simout1);

  counter = 0; %число совпадающих значений
  for ind=1:length(u0)
    if u0(ind) == u1(ind)
      counter = counter + 1;
    end
  end
  u(k) = counter / length(u0); %вероятность корректного сигнала

  %u(k) = 1 - sum(xor(u0, u1)) / length(u0);
  disp(j);
  disp(k);
 end
 %оценка показателя (реакции) по выборке наблюдений
 P_O = mean(u);
 Y(j)=P_O;
end
%определение коэффициентов регрессии
C=X*X';
b_ = inv(C)*X*Y';
%формирование зависимости реакции системы на множестве
%значений факторов
A=minf(1):0.1:maxf(1);
B=minf(2):0.1:maxf(2);
[k N1]=size(A);
[k N2]=size(B);
for i=1:N1
 for j=1:N2
  an(i)=2*(A(i)-minf(1))/(maxf(1)-minf(1))-1;
  bn(j)=2*(B(j)-minf(2))/(maxf(2)-minf(2))-1;
 %экспериментальная поверхность реакции
 Yc(j,i)=b_(1)+an(i)*b_(2)+bn(j)*b_(3)+an(i)*bn(j)*b_(4);
 end
end

%% 2.1 Задание интервала дискретизации по времени и количества отсчетов
%на интервале моделирования [0,10] с.
Ts=0.001;
Ns=10000;
% Оценка L в зависимости
NP=0.2
nf=2;
% format mr, R
minf=[0.007 1];
maxf=[0.01 7];
%формирование дробного двухуровневого плана эксперимента
%для учета взаимодействий
fracfact('a b ab' );
N=2^nf;

fracplan=ans;
fictfact=ones(N,1);
X=[fictfact ans]';
fraceks=zeros(N,nf);
for i=1:nf
  for j=1:N
    fraceks(j,i)=minf(i)+(fracplan(j,i)+1)*(maxf(i)-minf(i))/2;
  end
end
fraceks
%% 2.2 Планирование
% тактическое планирование эксперимента
%задание доверительного интервала и уровня значимости
dl=0.01;
alpha=0.06;
%определение t-критического
tkr_alpha=norminv(1-alpha/2);
%цикл по совокупности экспериментов стратегического плана
for j=1:N
  a=fraceks(j,1);
  b=fraceks(j,2);
  mr=a;
  R=b;
  %организация цикла статистических испытаний с переменным объемом
  %выборки для достижения заданной точности оценки показателя
  NE=1;
  l=0;
  SQ=0;
  D=1;% выборочная дисперсия
  u = [];
  while NE < tkr_alpha^2*D/dl^2
    %имитация функционирования системы
    to=round(rand*100); %инициализация генератора шума
    sim('trenl',Ts*Ns);
    % Подсчет числа ошибок

    u0 = simout;
    u1 = simout1;
    %u1 = squeeze(simout1);

    %u(NE) = sum(xor(u0, u1)) / length(u0);
    counter = 0; %число различающихся значений
    for ind = 1:length(u0)
      if u0(ind) ~= u1(ind)
        counter = counter + 1;
      end
    end
    u(NE) = counter / (Ts*Ns);
    l=l+u(NE);
    SQ=SQ+u(NE)^2;
    if NE > 20 D = SQ / (NE-1) - (l^2) / (NE * (NE-1)); end
    NE=NE+1;
  end
  NE= NE - 1;
  %оценка показателя (реакции) по выборке наблюдений
  L=mean(u);
  Yl(j)=L;
end
%определение коэффициентов регрессии
Cl=X*X';
b_l=inv(Cl)*X*Yl'
%формирование зависимости реакции системы на множестве
%значений факторов
Al=minf(1):0.0001:maxf(1);
Bl=minf(2):0.01:maxf(2);
[k N1]=size(Al);
[k N2]=size(Bl);
for i=1:N1
  for j=1:N2
    anl(i)=2*(Al(i)-minf(1))/(maxf(1)-minf(1))-1;
    bnl(j)=2*(Bl(j)-minf(2))/(maxf(2)-minf(2))-1;
    %экспериментальная поверхность реакции
    Yo(j,i)=b_l(1)+anl(i)*b_l(2)+bnl(j)*b_l(3)+anl(i)*bnl(j)*b_l(4);
  end
end

%% 3 Отображение зависимостей в трехмерной графике
[x,y]=meshgrid(A,B);
[xl,yl]=meshgrid(Al,Bl);
figure;
subplot(1,2,1),
plot3(x,y,Yc),
xlabel('мощность шума NP'),
ylabel('радиус R'),
zlabel('реакция'),
title('Зависимость реакции от R и NP'),
grid on,
subplot(1,2,2),
plot3(xl,yl,Yo),
xlabel('Коэффициент различимости mr'),
ylabel('радиус R'),
zlabel('L'),
title('зависимость интенсивности ложных тревог L от расстояния от R и mr'),
grid on;