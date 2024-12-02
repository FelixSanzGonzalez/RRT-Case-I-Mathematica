syms q   % Definir q como variable simbólica
f = q*(1/(1000-600*q)) + (1-q)*(((1/(10^3*(1-((600*(1-q))/10^3))))*(1-(((600*(1-q))/10^3)/2))));   % Definir la función
df = diff(f, q);     % Derivar la función
sol = solve(df == 0, q); % Resolver la derivada igualada a 0
sol_num = vpa(sol);  % Convertir la solución simbólica a numérica
disp(sol_num)        % Mostrar las soluciones numéricas
