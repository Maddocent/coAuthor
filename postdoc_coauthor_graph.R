#install.packages("RISmed")
library(RISmed)

#set up initial objects for compiling publication lists
authlist<-list()
pmid.rslt<-character()
obj<-list(authlist=authlist, pmid.rslt=pmid.rslt)

#This function retrieves publications for each author
add_pubs<-function(postdoc) {
  rslt.new<-EUtilsGet(postdoc, type="efetch")
  rslt.chk<-!(PMID(rslt.new) %in% obj$pmid.rslt)
  
  authlist.new<-Author(rslt.new)
  authlist.add<-authlist.new[unlist(rslt.chk)]
  
  obj$authlist<-c(obj$authlist, authlist.add)
  obj$pmid.rslt<-c(obj$pmid.rslt, PMID(rslt.new)[rslt.chk])
  
  print(c(length(authlist.new)," ",length(authlist.add)))
  
  return<-list(authlist = obj$authlist,  pmid.rslt=obj$pmid.rslt)
}

#Add people here using the PubMed search queries
#pmq creates the query, then add_pubs performs the query and adds them to the existing publication list
pmq<-EUtilsSummary(query="teunis ma", db="pubmed")
obj<-add_pubs(pmq)
  
pmq2<-EUtilsSummary(query="pieters rhh, db="pubmed")
obj<-add_pubs(pmq2)

pmq3<-EUtilsSummary(query="krul cam", db="pubmed")
obj<-add_pubs(pmq3)

pmq4<-EUtilsSummary(query="vaessen s", db="pubmed")
obj<-add_pubs(pmq4)

#pmq5<-EUtilsSummary(query="verkoeijen s", db="pubmed")
#obj<-add_pubs(pmq5)

#pmq6<-EUtilsSummary(query="ten klooster jp", db="pubmed")
#obj<-add_pubs(pmq6)

authlist<-obj$authlist


library(plyr)
library(stringr)

#install.packages("gtools")
library(gtools)


#Data cleaning step - lots of variations on names!
postdoc<-matrix(ncol=2, nrow=0)
for (i in 1:length(authlist)){
  authlist[[i]]$lastname<-str_replace(string=authlist[[i]]$LastName, pattern="Seltzer",replacement="Mailick")
  authlist[[i]]$firstname<-word(string=authlist[[i]]$ForeName) 
  authlist[[i]]$name<-paste( authlist[[i]]$firstname,   authlist[[i]]$lastname, sep=" ")
  authlist[[i]]$name<-ifelse(authlist[[i]]$name %in% c("Christopher Cuniff", "Chris Cunniff"), "Christopher Cunniff", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Julie Lounds", "Julie Taylor", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "A Woodman", "Ashley Woodman", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "P Hauser-Cram", "Penny Hauser-Cram", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "M Mailick", "Marsha Mailick", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Marsha Mailick Mailick", "Marsha Mailick", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "A Esbensen", "Anna Esbensen", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "J Greenberg", "Jan Greenberg", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Jeremy Veenstra-Vanderweele", "Jeremy Veenstra-VanderWeele", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "G Orsmond", "Gael Orsmond", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "J Rojahn", "Johannes Rojahn", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "B Benson", "Betsey Benson", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "S Bishop", "Somer Bishop", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "R Hemp", "Richard Hemp", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "S Hartley", "Sigan Hartley", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "J Hong", "Jinkuk Hong", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "W Maclean", "William MacLean", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "W MacLean", "William MacLean", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "William Maclean", "William MacLean", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "A Ben-Sasson", "Ayelet Ben-Sasson", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "M Kadlec", "Mary Kadlec", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Abba Kreiger", "Abba Krieger", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "A Carter", "Alice Carter", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "M Krauss", "Marty Krauss", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "S Cermak", "Sharon Cermak", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "D Sikora", "Darryn Sikora", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "D Braddock", "David Braddock", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "J Swaine", "Jamie Swaine", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "M Roach", "Mary Roach", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Chris Coe", "Christopher Coe", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "H Tager-Flusberg", "Helen Tager-Flusberg", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "S Parish", "Susan Parish", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "K Luken", "Karen Luken", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Sandy Magaa", "Sandra Magana", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Sandy Magaña", "Sandra Magana", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Sandy Magana", "Sandra Magana", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Sandra Maga?a", "Sandra Magana", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Ren?e Lockhart", "Renee Lockhart", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Rene Lockhart", "Renee Lockhart", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Zack Warren", "Zachary Warren", authlist[[i]]$name)
  authlist[[i]]$name<-ifelse(authlist[[i]]$name == "Kim van Naarden Braun", "Kim Van Naarden Braun", authlist[[i]]$name)
  #drop single-author papers... it's prestigious, but it screws up the code.
  b<-if(length(authlist[[i]]$name) > 1) combinations(n=length(authlist[[i]]$name), r=2, v=authlist[[i]]$name) else matrix(ncol=2, nrow=0)
  postdoc<-rbind(postdoc, b)
}

#take a look to manually catch duplicates or errors. 
table(postdoc, useNA="always")

#manually add citations, for people who aren't yet connected to the group (i.e., in-press papers)
addtl<-read.csv("c:/Users/maenner/Downloads/Postdoc Coauthor Network - Sheet1.csv",head=TRUE)

matchmake<-function(authors){
  if(length(authors) > 1) combinations(n=length(authors), r=2, v=as.vector(authors)) else matrix(ncol=2, nrow=0)
}

supp.list<-ddply(.data=addtl, .variables="papernum", summarize, matchmake(AuthorFirstNameLastName))
add.list<-data.frame(cbind(supp.list[,2][,1], supp.list[,2][,2]))
colnames(add.list)<-c("V1","V2")

postdoc<-rbind(postdoc, add.list)
postdoc$V1<-as.character(postdoc$V1)
postdoc$V2<-as.character(postdoc$V2)

#make a variable to indicate whether someone is part of the group
postdoc.list<-c("Matthew Maenner", "Jen Wong", "Ashley Woodman", "Leann Smith", "Marsha Mailick", "Jan Greenberg", "Anna Esbensen", "Julie Taylor",
                "Paul Shattuck", "Susan Parish", "Audra Sterling", "Sigan Hartley", "Eun Ha Namkung", "Kristy Anderson", "Jinkuk Hong", "Jieun Song", 
                "Jason Baker","Somer Bishop", "Gael Orsmond", "Renee Makuch", "Erin Barker", "Sandra Magana")


all.authors<-data.frame(author=unique(c(as.character(postdoc$V1), as.character(postdoc$V2))))
all.authors$OneOfUs<-ifelse(all.authors$author %in% postdoc.list, 1, 0)

#Convert to an edge list
#install.packages("igraph")
library(igraph)
gr<-graph.data.frame(postdoc, directed=FALSE, vertices=all.authors)
write.graph(gr, file="/Users/matt/proj/coAuthor/gr.graphml", format="GraphML")        
