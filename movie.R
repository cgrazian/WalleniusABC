################
### Import data
################

library(gdata)

movie.rat=read.xls("~/Dropbox/Wallenius/Data/Movies/ml-latest-small/ratings.xls",1)
str(movie.rat)

movie.gen=read.xls("~/Dropbox/Wallenius/Data/Movies/ml-latest-small/movies.xls",1)
str(movie.gen)

movie.gen[1,]
movie.rat[1,]

#######################
### Merge the datasets
#######################

movies <- merge ( movie.rat , movie.gen , by="movieId")
movies[1,]
str(movies)

attach(movies)

grep("Animation",movies$genres)
movies[65522,]

movies$genre <- ifelse( genres=="Action" , "Action", NA)
movies$genre <- ifelse( genres=="Adventure" , "Adventure", movies$genre)
movies$genre <- ifelse( genres=="Animation" , "Animation", movies$genre)
movies$genre <- ifelse( genres=="Children" , "Children", movies$genre)
movies$genre <- ifelse( genres=="Comedy" , "Comedy", movies$genre)
movies$genre <- ifelse( genres=="Crime" , "Crime", movies$genre)
movies$genre <- ifelse( genres=="Documentary" , "Documentary", movies$genre)
movies$genre <- ifelse( genres=="Drama" , "Drama", movies$genre)
movies$genre <- ifelse( genres=="Fantasy" , "Fantasy", movies$genre)
movies$genre <- ifelse( genres=="Film-Noir" , "Film-Noir", movies$genre)
movies$genre <- ifelse( genres=="Horror" , "Horror", movies$genre)
movies$genre <- ifelse( genres=="Mystery" , "Mystery", movies$genre)
movies$genre <- ifelse( genres=="Musical" , "Musical", movies$genre)
movies$genre <- ifelse( genres=="Romance" , "Romance", movies$genre)
movies$genre <- ifelse( genres=="Sci-Fi" , "Sci-Fi", movies$genre)
movies$genre <- ifelse( genres=="Thriller" , "Thriller", movies$genre)
movies$genre <- ifelse( genres=="War" , "War", movies$genre)
movies$genre <- ifelse( genres=="Western" , "Western", movies$genre)

table(movies$genre,exclude=NULL)

movies$genre[grep("Drama",movies$genres)] = "Drama"
movies$genre[grep("Comedy",movies$genres)] = "Comedy"

table(movies$genre,exclude=NULL)

movies$genre[grep("Thriller",movies$genres)] = "Thriller"
movies$genre[grep("Romance",movies$genres)] = "Romance"
movies$genre[grep("Western",movies$genres)] = "Western"
movies$genre[grep("War",movies$genres)] = "War"

table(movies$genre,exclude=NULL)

movies$genre[grep("Adventure",movies$genres)] = "Adventure"
movies$genre[grep("Horror",movies$genres)] = "Horror"
movies$genre[grep("Fantasy",movies$genres)] = "Fantasy"
movies$genre[grep("Crime",movies$genres)] = "Crime"
movies$genre[grep("Action",movies$genres)] = "Action"
movies$genre[grep("Musical",movies$genres)] = "Musical"

table(movies$genre,exclude=NULL)

movies$genre[grep("Documentary",movies$genres)] = "Documentary"
movies$genre[grep("Mystery",movies$genres)] = "Mystery"
movies$genre[grep("Film-Noir",movies$genres)] = "Film-Noir"
movies$genre[grep("Sci-Fi",movies$genres)] = "Sci-Fi"
movies$genre[grep("Animation",movies$genres)] = "Animation"

movies$genre <- ifelse( genres=="(no genres listed)" , "Comedy", movies$genre)
table(movies$genre,exclude=NULL)
m.tab=table(movies$genre,exclude=NULL)

save.image("~/Dropbox/2017_2018_Oxford/Wallenius/Data/Movies/movies.RData")

###########################################
### Creation of the matrix of observations
###########################################

detach(movies)
attach(movies)

movies$rating <- as.numeric(movies$rating)

categories=c("Action" , "Adventure" , "Animation" , "Children" ,
				"Comedy" , "Crime" , "Documentary" , "Drama" , "Fantasy" ,
				"Film-Noir" , "Horror" , "Musical" , "Mystery" , "Romance" ,
				"Sci-Fi" , "Thriller" , "War" , "Western")
str(movies)

detach(movies)
attach(movies)

### Let's create the matrix of the observations

min(userId)
max(userId)

movies[userId==1,]
nrow(movies[userId==1 , ] )
nrow(movies[userId==1 & genre=="Action" , ] )
nrow(movies[userId==1 & genre=="Action" & rating>7.5 , ] )
nrow(movies[userId==1 & genre==categories[1] & rating>7.5 , ] )
nrow(movies[userId==1 & genre==categories[5] & rating>7.5 , ] )

x.mat <- matrix( NA, nrow=max(userId) , ncol=18)

for (i in min(userId):max(userId))
{
	for (j in 1:18)
	{
		x.mat[i,j] <- nrow(movies[userId==i & 
						genre==categories[j] & rating>3.5 , ]	)
	}
	print(i)
}

apply(x.mat,1,sum)

colnames(x.mat) <- categories
head(x.mat)
usernames <- c()
for(i in 1:nrow(x.mat))
{
	usernames[i] <- paste("user",i,sep="")
}
rownames(x.mat) <- usernames

save.image("~/Dropbox/2017_2018_Oxford/Wallenius/Data/Movies/movies.RData")

write.csv(x.mat, "~/Dropbox/2017_2018_Oxford/Wallenius/Data/Movies/movies.csv")

### Let's create the vector of m's

m.vec <- c()

for(i in 1:length(categories))
{
	m.vec[i] <- nrow(movies[movies$genre==categories[i],])
}

save.image("~/Dropbox/Wallenius/Data/Movies/movies.RData")

### Let's create the vector of n's

n.vec <- c()

for(i in min(userId):max(userId))
{
	n.vec[i] <- nrow(movies[userId==i,]) 
}

save.image("~/Dropbox/Wallenius/Data/Movies/movies.RData")

library(BiasedUrn)

choose.eps=ABC.wallenius.omega.eps(x.mat,m=m.vec,n=n.vec,S=10^5)

length(unique(choose.eps$dist))
hist(choose.eps$dist,nclass=50)
quantile(choose.eps$dist,p=0.05)

save.image("~/Dropbox/Wallenius/Data/Movies/movies.RData")

abc.movies=ABC.wallenius.omega(x.mat,m=m.vec,n=n.vec,S=10^3,eps=0.5)
			
save.image("~/Dropbox/Wallenius/Data/Movies/movies.RData")

omega.movies.means=apply(abc.movies$omega,2,mean)
categories[rev(order(omega.movies.means))]

categories[omega.movies.means==min(omega.movies.means)]


cbind(categories[rev(order(omega.movies.means))],
		round(sort(omega.movies.means,decreasing=T),digits=3))

omega.norm=matrix(abc.movies$omega, nrow=nrow(abc.movies$omega), ncol=ncol(abc.movies$omega))
for(i in 1:nrow(omega.norm))
{
	omega.norm[i,] <- omega.norm[i,] / omega.norm[i,1]
}

omega.norm <- matrix(abc.movies$omega, nrow=nrow(abc.movies$omega), ncol=ncol(abc.movies$omega))

omega.norm.means <- apply(omega.norm,2,mean)
cbind(categories[rev(order(omega.norm.means))],
		round(sort(omega.norm.means,decreasing=T),digits=3))
omega.norm.sd <- apply(omega.norm,2,sd)
cbind(categories[rev(order(omega.norm.means))],
		round(sort(omega.norm.means,decreasing=T),digits=3),
		round(omega.norm.sd[rev(order(omega.norm.means))],digits=3))


omega.movies.sd=apply(abc.movies$omega,2,sd)
omega.movies.ci=apply(abc.movies$omega,2,quantile,prob=c(0.05,0.95))
omega.movies.ci=t(omega.movies.ci)
dim(omega.movies.ci)

cbind(categories[rev(order(omega.movies))],
		round(omega.movies.ci[rev(order(omega.movies)),1],digits=2),
		round(omega.movies[rev(order(omega.movies))],digits=2),
		round(omega.movies.ci[rev(order(omega.movies)),2],digits=2),
		round(omega.movies.sd[rev(order(omega.movies))],digits=2)		
		)
		
dim(abc.movies$omega)
col.movies <- c("green", "turquoise","tomato", "orange", "red", "darkolivegreen",
				"grey", "blue", "black","cyan", "gold", "grey28",
				"pink", "yellow", "purple", "darkorchid","chocolate","indianred1")

tiff("/Users/claragrazian/Dropbox/2017_2018_Oxford/Wallenius/Data/Movies/movies.tiff",width=6, height=6.5, units="in",res=300)

plot(density(omega.norm[,1],from=0, to=0.5),ylim=c(0,17),xlim=c(0,0.6), col=col.movies[1],
		main="Movies - Posterior distributions",xlab="")
for(i in 2:18)
{
	lines(density(omega.norm[,i],from=0, to=0.5),col=col.movies[(i-1)])
}
legend("topright",legend=categories[1:18],col=col.movies,lwd=2)

dev.off()

col.movies.bw <- c(rep("black",6) ,
					rep("grey36",6) ,
					rep("grey72",6)
)
movies.lty <- c(rep(1:6,3))

tiff("/Users/claragrazian/Dropbox/2017_2018_Oxford/Wallenius/Data/Movies/movies_bw.tiff",width=6, height=6.5, units="in",res=300)

plot(density(omega.norm[,1],from=0, to=0.5),ylim=c(0,17),xlim=c(0,0.6), 
			col=col.movies.bw[1], lty=movies.lty[1],
			main="Movies - Posterior distributions",xlab="")
for(i in 2:18)
{
	lines(density(omega.norm[,i],from=0, to=0.5),
			col=col.movies.bw[i],lty=movies.lty[i]
			)
}
legend("topright",legend=categories[1:18],col=col.movies.bw,
		lty=movies.lty,lwd=2)

dev.off()

save.image("~/Dropbox/Wallenius/Data/Movies/movies.RData")

obj <- density(omega.norm[,1],from=0, to=1)
print(obj)