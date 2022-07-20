# Big Data - Exercise 3 (Hackathon)
# GitHub: https://github.com/JuditHalperin/BigData/tree/main/Ex3

# Yudit Halperin - 324216589
# Ortal Calfon (Peretz) - 315011189
# Asnat Berlin - 211825401
# Naama Shenberger - 211983747

# Queries for questions a-g:


# a. How many users?
select count(*) from BX_db.BX_Users;
# b. How many books?
select count(*) from BX_db.BX_Books;
# c. How many ratings?
select count(*) from BX_db.BX_Book_Ratings;


# d. Histogram of user-ratings (how many users have rated N times?)
create view temp as select count(*) as num_ratings
from BX_db.BX_Users natural join BX_db.BX_Book_Ratings
group by User_ID
order by count(*);

select num_ratings, count(*) as bin_size
from temp
group by num_ratings
order by num_ratings;


# e. Histogram of book-ratings (how many books have been rated N times?)
create view booksHistogram as select count(*) as num_ratings
from BX_db.BX_Book_Ratings
group by ISBN
order by count(*);

select num_ratings, count(*) as bin_size
from booksHistogram
group by num_ratings
order by num_ratings;


# f. Top-10 rated books?
select Book_Title, count(Book_Rating)
from BX_db.BX_Books natural join BX_db.BX_Book_Ratings
group by ISBN
order by count(Book_Rating) desc
limit 10;


# g. Top-10 active users?
select rating.User_ID as name, count(User_ID) as N
from BX_db.BX_Book_Ratings rating
group by rating.User_ID
order by count(*) desc
limit 0, 10;