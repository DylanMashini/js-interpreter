function sqrt(value) {
    if (value < 0) {
        return null;
    }
    var x = value;
    var lastX = 0;
    while (x !== lastX) {
        lastX = x;
        x = (x + value / x) / 2;
    }
    return x;
}

function isPrime(num) {
    if (num <= 1) {
        return false;
    }
    if (num % 2 == 0 && num > 2) {
        return false;
    }
    for (var i = 3; i <= sqrt(num); i += 2) {
        if (num % i == 0) {
            return false;
        }
    }
    return true;
}

function findPrimes(limit) {
    for (let number = 1; number <= limit; number++) {
        if (isPrime(number)) {
            print(number);
        }
    }
}

findPrimes(100);