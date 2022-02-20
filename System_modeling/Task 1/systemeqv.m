function u=systemeqv(sigma, mu)
%логнормальное распределение с параметрами масштаба и формы a, b
alpha = rand(12, 1); 
u = sigma * (sum(alpha) - 6) + mu;
