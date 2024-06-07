// High order function
function highOrderFunction(f, x) {
    return f(x);
}

console.log(highOrderFunction(x => x + 1, 1));

// Hidden function
const hiddenFunction = (x) => x + 1;

console.log(hiddenFunction(1));

// Process List
function processList(list, f) {
    return list.map(f);
}