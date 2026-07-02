// A set of semi-real world use-cases. These should eventually be moved to crosscheck, once the VM
// supports more of this.
#[cfg(test)]
mod tests {
    use crate::treewalk::test_utils::*;

    #[test]
    fn word_count() {
        let input = r#"
def word_count(text):
    counts = {}

    for word in text.split():
        if word in counts:
            counts[word] += 1
        else:
            counts[word] = 1

    return counts

word_count("rust python rust")
"#;
        assert_eval_eq!(
            input,
            dict!({ str!("rust") => int!(2), str!("python") => int!(1) })
        );
    }

    #[test]
    fn freq_table_max_lookup() {
        let input = r#"
def most_common(text):
    counts = {}

    for word in text.split():
        if word in counts:
            counts[word] += 1
        else:
            counts[word] = 1

    best_word = None
    best_count = 0

    for word in counts:
        if counts[word] > best_count:
            best_word = word
            best_count = counts[word]

    return best_word, best_count

most_common("red blue red green red blue")
"#;
        assert_eval_eq!(input, tuple![str!("red"), int!(3)]);
    }

    #[test]
    fn grouping_into_buckets() {
        let input = r#"
def group_by_length(words):
    groups = {}

    for word in words:
        n = len(word)

        if n not in groups:
            groups[n] = []

        groups[n].append(word)

    return groups

group_by_length(["a", "bb", "cc", "ddd"])
"#;
        assert_eval_eq!(
            input,
            dict!({ int!(1) => list![str!("a")],
                int!(2) => list![str!("bb"), str!("cc")],
                int!(3) => list![str!("ddd")] })
        );
    }

    #[test]
    fn filter_plus_transform() {
        let input = r#"
def long_words(text):
    result = []

    for word in text.split():
        if len(word) >= 4:
            result.append(word.upper())

    return result

long_words("the quick brown fox jumps")
"#;
        assert_eval_eq!(input, list![str!("QUICK"), str!("BROWN"), str!("JUMPS")]);
    }

    #[test]
    fn nested_loop_aggregation() {
        let input = r#"
def multiplication_table(n):
    total = 0

    for i in range(n):
        for j in range(n):
            total += i * j

    return total

multiplication_table(4)
"#;
        assert_eval_eq!(input, int!(36));
    }

    #[test]
    fn inventory_report() {
        let input = r#"
def inventory_total(items):
    totals = {}

    for name, qty in items:
        if name in totals:
            totals[name] += qty
        else:
            totals[name] = qty

    return totals

inventory_total([
    ("apple", 2),
    ("banana", 1),
    ("apple", 3),
])
"#;
        assert_eval_eq!(
            input,
            dict!({ str!("apple") => int!(5), str!("banana") => int!(1) })
        );
    }

    #[test]
    fn reverse_lookup() {
        let input = r#"
def reverse_lookup(d):
    result = {}

    for key in d:
        result[d[key]] = key

    return result

reverse_lookup({
    "a": 1,
    "b": 2,
    "c": 3,
})
"#;
        assert_eval_eq!(
            input,
            dict!({ int!(1) => str!("a"), int!(2) => str!("b"), int!(3) => str!("c") })
        );
    }

    #[test]
    fn class_counter() {
        let input = r#"
class Counter:
    def __init__(self):
        self.value = 0

    def inc(self):
        self.value += 1

c = Counter()
c.inc()
c.inc()

c.value
"#;
        assert_eval_eq!(input, int!(2));
    }

    #[test]
    fn class_bank_account() {
        let input = r#"
class BankAccount:
    def __init__(self, balance):
        self.balance = balance

    def deposit(self, amount):
        self.balance += amount

account = BankAccount(100)
account.deposit(25)

account.balance
"#;
        assert_eval_eq!(input, int!(125));
    }
}
