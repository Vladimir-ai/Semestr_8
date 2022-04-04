clear all;

% Theoretical values;
% http://window.edu.ru/resource/208/29208/files/samiit225.pdf
% queue_len
m = 6;

% Channel_num
n = 4;

fprintf('========= Theoretical values =========\n\n');

% Service request per second
lambda = 320 / 24 / 60 / 60; % by default, per second
fprintf('Mean value of calls per second: %f \n', lambda);

% Service amount, per second
mu = 1 / (5 * 60); % by default, per second
fprintf("Service amount per second: %f \n", mu);

% traffic intensity
rho = lambda / mu;
fprintf('Traffic intensity: %f\n', rho);

p_0 = 1;
for i=1:n
  p_0 = p_0 + rho.^i / factorial(i);
end
p_0 = 1 / (p_0 + rho.^(n + 1) / (n * factorial(n)) * ((1 - rho / n).^m / (1 - rho / n)));

% Idle probability
fprintf('Idle probability: %f\n', p_0);

% Reject probability
p_reject = p_0 * (rho.^(n + m) / (n.^m * factorial(n)));
fprintf('Reject probability: %f\n', p_reject);

% Relative throughput
q = 1 - p_reject;
fprintf('Relative throughput: %f\n', q);

% Absolute throughput
Q = q * lambda;
fprintf('Absolute throughput: %f calls per second\n', Q);

% Mean amount of occupied phones
n_mean = Q / mu;
fprintf('Mean amount of occupied phones: %f\n', n_mean);

% mean amount of waiting requests in queue
e_wait = (rho.^(n + 1)) / (n * factorial(n)) * p_0 * (1 - (rho / n).^m * (1 + m * (1 - rho / n))) / (1 - rho / n).^2;
fprintf('mean amount of waiting requests in queue: %f\n\n', e_wait);


% Experimental values
day_amount = 10;
fprintf('========= Experimental values, days: %d =========\n\n', day_amount);

time_per_day = 24 * 3600; % day in seconds
total_time = time_per_day * day_amount;

simOut = sim('lab3', total_time);

generated_values = get(simOut, 'GeneratedEntities');
fprintf("Total generated: %d, mean generated per day %f\n", max(generated_values), max(generated_values) / day_amount);

call_time = get(simOut, 'CallTime');
fprintf("Mean call time: %f\n", mean(call_time));

phone_ocupancy = get(simOut, 'PhoneOccupancy');
fprintf("Mean phone ocupancy: %f\n", mean(phone_ocupancy));

p_0_exp = nnz(~phone_ocupancy.Data) / numel(phone_ocupancy.Data);
fprintf("Idle probability: %f\n", p_0_exp);

queue_ocupancy = get(simOut, 'NumberOfEntitiesInQueue');
fprintf("Mean amount of entities in queue : %f\n", mean(queue_ocupancy.Data));

processed_values = get(simOut, 'ProcessedCalls');

fprintf('Reject probability: %f\n', 1 - numel(processed_values.Data) / numel(generated_values.Data));

fprintf("Processed all calls: %d\n\n", sum(processed_values) == sum(generated_values));