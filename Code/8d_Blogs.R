# 4. Blogs

# 4.a. Read in blogs
blogs<-data.frame(country=NA,blog=NA)

for (country in countrylevels) {
  rawblog<-readLines(paste0("Blogs/",country," Blog.txt",sep=""))
  nrawblog <- !nchar(rawblog)
  out <- rbind.fill(lapply(split(rawblog[!nrawblog], cumsum(nrawblog)[!nrawblog]),
                           function(rawblog) data.frame(t(rawblog))))
  cleaned_blog<- as.character(gsub("\\[.+?\\]","",out$t.rawblog.))
  cleaned_blog<- gsub("\\width.+?\\height","height",cleaned_blog)
  cleaned_blog<- gsub("\\height.+?\\/>","height=\"400\" /> <br/>",cleaned_blog)
  cleaned_blog<- paste(cleaned_blog,collapse="<br/><br/>")
  cleaned_blog<- gsub("height=\"400\" /> <br/>","height=\"400\" />",cleaned_blog)
  
  blogs <- rbind(blogs,data.frame(country=country,blog=cleaned_blog)) 
  #assign(paste0(gsub(" ","",country),"_blog",sep=""),cleaned_blog)
  rm(rawblog,nrawblog,cleaned_blog,out)
}
rm(country)
blogs<-blogs[!is.na(blogs$country),]

