package day08

trait Store[A] {
    def store(key: A): Boolean // True if added, False otherwise
    def contains(key:A): Boolean
    def length: Int
}