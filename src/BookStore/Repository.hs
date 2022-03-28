module BookStore.Repository where
    
import BookStore.Models

class BookRepository a where
    getBook :: a -> Int -> Either String Book
    getBooks :: a -> Either String [Book]

