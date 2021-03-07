bool enoughBalance(User, Item) {
    return User.balance >= Item.price;
}

bool oldEnough(User, Item) {
    return User.age >= Item.min_age;
}

function validate(User, Item, Validators[]) {
  for (int i = 0; i < tests.length; i++) {
    if (!tests[i](obj)) {
      return false;
    }
  }

  return true;
}
