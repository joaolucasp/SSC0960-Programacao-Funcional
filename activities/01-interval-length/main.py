# This problem is: Faça um programa em Python, utilizando os conceitos de programação do paradigma funcional, que leia dois inteiros x e y, sendo que x é menor que y, e imprima o comprimento do maior intervalo entre dois números primos consecutivos maiores ou iguais a x e menores ou iguais a y.

def read_x_and_y():
    x = int(input())
    y = int(input())
    return x, y

def is_prime(number):
    if number < 2:
        return False
    for i in range(2, number):
        if number % i == 0:
            return False
    return True

def next_prime(number):
    if is_prime(number):
        return number
    
    return next_prime(number + 1)


def prime_intervals(x, y):
    def calculate_interval(prime1, prime2, max_interval):
        if prime2 > y:
            return max_interval
        interval = prime2 - prime1
        if interval > max_interval:
            max_interval = interval
        return calculate_interval(prime2, next_prime(prime2 + 1), max_interval)

    prime1 = next_prime(x)
    prime2 = next_prime(prime1 + 1)
    
    if prime2 > y:
        return 0
    
    max_interval = prime2 - prime1

    return calculate_interval(prime2, next_prime(prime2 + 1), max_interval)

values = read_x_and_y()

if values[0] >= values[1]:
    print("Invalid values. x must be less than y.")
else:
    print(prime_intervals(values[0], values[1]))