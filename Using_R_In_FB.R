#library(devtools)
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")



library(Rfacebook)
token <- ""

user_name = ""

df <- getUsers(user_name, token, private_info = TRUE)
df$name #name
df$hometown # my hometown



my_likes <- getLikes(user= "#" token=token)



fans <- getFriends(token, simplify = TRUE)

my_friends_info <- getUsers(my_friends$id, token, private_info = TRUE)
table(my_friends_info$gender)  # gender

table(substr(my_friends_info$locale, 1, 2))  # language
table(substr(my_friends_info$locale, 4, 5))  # country

mat <- getNetwork(token, format = "adj.matrix")
dim(mat)

# get page info 
#True will make it pull user info 
# account info 

pg.posts <- getPage(user_name, token, n=100, feed=TRUE)


blah <- getUsers(pg.posts$from_id, token, private_info = TRUE)
fix(blah)

getCheckins(562008467, n = 10, token, tags = FALSE)


#pulled this from above 
## Getting information about Facebook's Facebook Page
fb_page <- getPage(page=user_name, token=token)
## Getting information and likes/comments about most recent post
post <- getPost(post=fb_page$id[1], n=200, token=token)
## End(Not run)
post

## Searching 100 public posts that mention
posts <- searchFacebook( string=user_name, token=token, n=100 )
## Searching 100 public posts that mention "facebook" from yesterday
posts <- searchFacebook( string=user_name, token=token, n=100 ,
                         since = "yesterday 00:00", until = "yesterday 23:59")





#graph social network on FB

me <- getUsers("me", token=token)
my_friends <- getFriends(token=token, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)
my_network <- getNetwork(token, format="adj.matrix")
singletons <- rowSums(my_network)==0 # friends who are friends with me alone

install.packages("igraph")
library(igraph)
my_graph <- graph.adjacency(my_network[!singletons,!singletons])
layout <- layout.drl(my_graph,options=list(simmer.attraction=0))
plot(my_graph, vertex.size=2, 
     #vertex.label=NA, 
     vertex.label.cex=0.5,
     edge.arrow.size=0, edge.curved=TRUE,layout=layout)