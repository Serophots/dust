let test = "beep boop";

if test == "beep boop" {
    print("boop beep"); //Bog standard comment
    // whoo
} else {
    5 + /* a snippet comment */ 7;
    print("foop fleep", 5.0 / 3.0);
    jeepers();
}

/// A doc comment on `jeepers`
fn jeepers() {
    print("jeepers");
}
