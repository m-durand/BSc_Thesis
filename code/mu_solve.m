steps = 100;
alpha005 = 0.05;
alpha095 = 0.95;

mu_prom_min = 92.9612;
mu_prom_max = 105.0416;

mu_int = (mu_prom_max - mu_prom_min)/steps;

xi_prom_min = -0.8394;
xi_prom_max = -0.05231;

xi_int = (xi_prom_max- xi_prom_min)/steps;

n = steps*steps;
aprox = zeros(n,5);


       p00 = -76.38  
       p10 = 1.494  
       p01 = 16.28  
       p20 = -0.006184 
       p11 = -0.1605  
       p02 = -0.1861  

  %sigmita <- p00 + p10*x + p01*y + p20*x**2 + p11*x*y + p02*y**2


c1 = 1;
c2 = 1;

for i=1:n
    i
    mu_eval = mu_prom_min + (c2-1)*mu_int;
    aprox(i,1) = mu_eval;
    xi_eval =xi_prom_min + (c1-1)*xi_int;
    aprox(i,2) = xi_eval;
    mu = mu_eval;
    xi = xi_eval;
    expo = -xi;
    expo2 = 1/xi
    sigma = p00 + p10*mu + p01*xi + p20*mu^2 + p11*mu*xi + p02*xi^2;
    aprox(i,3) = sigma;
    qalpha005 = mu - sigma*(-1*log(1-alpha005))^expo;
    qalpha095 = mu + sigma*(-1*log(1-alpha095))^expo2;
    aprox(i,4) = qalpha005;
    aprox(i,5) = qalpha095;
    %eqn = (-1*log(alpha))^(1*xi) == (1+ xi*(Qalpha - mu)/sigma)
    %eqn = log(alpha)^expo == -1 + xi*((Qalpha-mu)/sigma)
    
    %Qalpha = 126;
    %logo = log(1-alpha);
    %nlogo = -1*logo;
    %expo = xi_eval;
    %siginv = 1/sigma;
    %Mualpha = mu_eval;
    
    
    %eqn = Qalpha == mu_eval-(sigma/xi_eval) * (1 + (-1*logo)^expo);
    
    %eqn = Mualpha == Qalpha - sigma*nlogo^expo;
    
    %inexp = ((x-mu_eval)/sigma);
    %%eqn = sum(((1+xi_eval*inexp)^(expo-1))*siginv*exp(-1*(1+xi_eval*inexp)^expo)==84.72777);
    %solmu = xi*(Qalpha-mu)/(log(alpha)^expo-1);
   % solmu = solve(eqn,sigma);
    
    %aprox(i,3) = solmu;
    if c1<steps
        c1 = c1 + 1 ;
    else
        c1 = 1;
        c2 = c2 + 1;
    end
end

Q005=aprox(:,4);
Q095=aprox(:,5);

hist(Q005)
hist(Q095)