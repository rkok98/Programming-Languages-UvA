bool enoughBalance(User, Item) {
    return User.balance >= Item.price;
}

bool oldEnough(User, Item) {
    return User.age >= Item.min_age;
}

function validate(User, Item, Validators[]) {
  for (int i = 0; i < Validators.length; i++) {
    if (!Validators[i](User, Item)) {
      return false;
    }
  }

  return true;
}
