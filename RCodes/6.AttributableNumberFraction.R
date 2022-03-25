
exposed_5km<-subset(dat.run,exposure_5km==1)

round(sd(exposed_5km$Metric_5km)*Stroke_5km_GAM_NearestReplacement_dist0.1$summary,2)



AR_5km<-data.frame(EST=exposed_5km$Metric_5km*Stroke_5km_GAM_NearestReplacement_dist0.1$summary[1],
                     E95L=
                       exposed_5km$Metric_5km*Stroke_5km_GAM_NearestReplacement_dist0.1$summary[2],
                     E95H=
                       exposed_5km$Metric_5km*Stroke_5km_GAM_NearestReplacement_dist0.1$summary[3])

AR_5km$EST_F<-AR_5km$EST/exposed_5km$STROKE*100
AR_5km$E95L_F<-AR_5km$E95L/exposed_5km$STROKE*100
AR_5km$E95H_F<-AR_5km$E95H/exposed_5km$STROKE*100
AR_5km$STATE<-exposed_5km$STATE_FIPS
AR_5km$POP<-exposed_5km$Pop_18over
AR_5km$STROKE<-exposed_5km$STROKE
AR_5km$AFFGEOID<-exposed_5km$AFFGEOID

(AR_5km$E95H_F-AR_5km$EST_F)/1.96

se<-(Stroke_5km_GAM_NearestReplacement_dist0.1$summary[3]-Stroke_5km_GAM_NearestReplacement_dist0.1$summary[1])/1.96*exposed_5km$Metric_5km

ARfitdat<-data.frame(AR=AR_5km$EST,SE=se,AFFGEOID=AR_5km$AFFGEOID, AF=AR_5km$EST_F)

ARfitdat<-merge(ARfitdat,exposed_5km,by="AFFGEOID")


write.csv(ARfitdat,"/Users/kimdh/Dropbox (Yale_FES)/Natalia/Code/CensusTract/MethodPaper/AR_5km_fitdat.csv")






AR_5km<-AR_5km[order(AR_5km$STATE,AR_5km$EST_F, decreasing=TRUE),]
rownames(AR_5km)<-seq(nrow(AR_5km))

AR_5km_STATE_INDNUM<-as.numeric(rownames(AR_5km[!duplicated(AR_5km$STATE),]))
AR_5km_STATE_IND<-unique(AR_5km$STATE)

AR_5km_S<-vector("list",7)
AR_5km_S[[1]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[1])
AR_5km_S[[2]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[2])
AR_5km_S[[3]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[3])
AR_5km_S[[4]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[4])
AR_5km_S[[5]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[5])
AR_5km_S[[6]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[6])
AR_5km_S[[7]]<-subset(AR_5km,STATE==AR_5km_STATE_IND[7])












AR_5km$ALL<-1

ALL_AF_5km_1<-aggregate(AR_5km$EST*AR_5km$POP,by=list(AR_5km$ALL),FUN=sum)
ALL_AF_5km_2<-aggregate(AR_5km$E95L*AR_5km$POP,by=list(AR_5km$ALL),FUN=sum)
ALL_AF_5km_3<-aggregate(AR_5km$E95H*AR_5km$POP,by=list(AR_5km$ALL),FUN=sum)
ALL_AF_5km_4<-aggregate(AR_5km$STROKE*AR_5km$POP,by=list(AR_5km$ALL),FUN=sum)

colnames(ALL_AF_5km_1)[2] <- "EST"
colnames(ALL_AF_5km_2)[2] <- "E95L"
colnames(ALL_AF_5km_3)[2] <- "E95H"
colnames(ALL_AF_5km_4)[2] <- "STROKE"


ALL_AF_5km<-merge(ALL_AF_5km_1,ALL_AF_5km_2,by="Group.1")
ALL_AF_5km<-merge(ALL_AF_5km,ALL_AF_5km_3,by="Group.1")
ALL_AF_5km<-merge(ALL_AF_5km,ALL_AF_5km_4,by="Group.1")

ALL_AF_5km$EST<-ALL_AF_5km$EST/ALL_AF_5km$STROKE*100
ALL_AF_5km$E95L<-ALL_AF_5km$E95L/ALL_AF_5km$STROKE*100
ALL_AF_5km$E95H<-ALL_AF_5km$E95H/ALL_AF_5km$STROKE*100


STATE_AF_5km_1<-aggregate(AR_5km$EST*AR_5km$POP,by=list(AR_5km$STATE),FUN=sum)
STATE_AF_5km_2<-aggregate(AR_5km$E95L*AR_5km$POP,by=list(AR_5km$STATE),FUN=sum)
STATE_AF_5km_3<-aggregate(AR_5km$E95H*AR_5km$POP,by=list(AR_5km$STATE),FUN=sum)
STATE_AF_5km_4<-aggregate(AR_5km$STROKE*AR_5km$POP,by=list(AR_5km$STATE),FUN=sum)

colnames(STATE_AF_5km_1)[2] <- "EST"
colnames(STATE_AF_5km_2)[2] <- "E95L"
colnames(STATE_AF_5km_3)[2] <- "E95H"
colnames(STATE_AF_5km_4)[2] <- "STROKE"


STATE_AF_5km<-merge(STATE_AF_5km_1,STATE_AF_5km_2,by="Group.1")
STATE_AF_5km<-merge(STATE_AF_5km,STATE_AF_5km_3,by="Group.1")
STATE_AF_5km<-merge(STATE_AF_5km,STATE_AF_5km_4,by="Group.1")

STATE_AF_5km$EST<-STATE_AF_5km$EST/STATE_AF_5km$STROKE*100
STATE_AF_5km$E95L<-STATE_AF_5km$E95L/STATE_AF_5km$STROKE*100
STATE_AF_5km$E95H<-STATE_AF_5km$E95H/STATE_AF_5km$STROKE*100

STATE_AR_5km_1<-aggregate(AR_5km$EST*AR_5km$POP/100,by=list(AR_5km$STATE),FUN=sum)
STATE_AR_5km_2<-aggregate(AR_5km$E95L*AR_5km$POP/100,by=list(AR_5km$STATE),FUN=sum)
STATE_AR_5km_3<-aggregate(AR_5km$E95H*AR_5km$POP/100,by=list(AR_5km$STATE),FUN=sum)


colnames(STATE_AR_5km_1)[2] <- "EST"
colnames(STATE_AR_5km_2)[2] <- "E95L"
colnames(STATE_AR_5km_3)[2] <- "E95H"

STATE_AR_5km<-merge(STATE_AR_5km_1,STATE_AR_5km_2,by="Group.1")
STATE_AR_5km<-merge(STATE_AR_5km,STATE_AR_5km_3,by="Group.1")


STATE_AR_5km<-round(STATE_AR_5km,1)
STATE_AF_5km<-round(STATE_AF_5km,1)

StateName<-c("Alabama (AL)", "Arkansas (AR)", "Louisiana (LA)", "Mississippi (MS)", "New Mexico (NM)", "Oklahoma (OK)", "Texas (TX)")

STATE_AR_5km$StateName <- StateName
STATE_AF_5km$StateName <- StateName

write.csv(STATE_AR_5km,"ADD FOLDER NAME HERE/file/STATE_AR_5km_NEW.csv")
write.csv(STATE_AF_5km,"ADD FOLDER NAME HERE/file/STATE_AF_5km_NEW.csv")



library(ggplot2)
colormat_CI<-colormat<-matrix(NA,nrow=1,ncol=7)
for (aa in seq(7)) {
  colormat_CI[1,aa]<-rgb(0.2+aa*0.07,0.7,0.5-aa*0.07,0.3,maxColorValue = 1)
  colormat[1,aa]<-rgb(1-aa*0.07,0.3,0.1+aa*0.07,1,maxColorValue = 1)
}
colormat<-c(colormat)
colormat_CI<-c(colormat_CI)




add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}


LEGEND <- function (x, y = NULL, legend, fill = NULL, 
                    col = par("col"), pt.col=col, line.col=col,
                    border = "black", lty, lwd, pch, angle = 45, density = NULL,
                    bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"),
                    box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
                    xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0,
                                                                                0.5), text.width = NULL, text.col = par("col"), text.font = NULL,
                    merge = do.lines && has.pch, trace = FALSE, plot = TRUE,
                    ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.col = text.col,
                    title.adj = 0.5, seg.len = 2)
{
  if (missing(legend) && !missing(y) && (is.character(y) ||
                                         is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  if (!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd = op))
    par(xpd = xpd)
  }
  title <- as.graphicsAnnot(title)
  if (length(title) > 1)
    stop("invalid 'title'")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend))
    1
  else length(legend)
  if (n.leg == 0)
    stop("'legend' is of length 0")
  auto <- if (is.character(x))
    match.arg(x, c("bottomright", "bottom", "bottomleft",
                   "left", "topleft", "top", "topright", "right", "center"))
  else NA
  if (is.na(auto)) {
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2)
      stop("invalid coordinate lengths")
  }
  else nx <- 0
  xlog <- par("xlog")
  ylog <- par("ylog")
  rect2 <- function(left, top, dx, dy, density = NULL, angle,
                    ...) {
    r <- left + dx
    if (xlog) {
      left <- 10^left
      r <- 10^r
    }
    b <- top - dy
    if (ylog) {
      top <- 10^top
      b <- 10^b
    }
    rect(left, top, r, b, angle = angle, density = density,
         ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    if (xlog) {
      x1 <- 10^x1
      x2 <- 10^x2
    }
    y2 <- y1 + dy
    if (ylog) {
      y1 <- 10^y1
      y2 <- 10^y2
    }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    if (xlog)
      x <- 10^x
    if (ylog)
      y <- 10^y
    text(x, y, ...)
  }
  if (trace)
    catn <- function(...) do.call("cat", c(lapply(list(...),
                                                  formatC), list("\n")))
  cin <- par("cin")
  Cex <- cex * par("cex")
  if (is.null(text.width))
    text.width <- max(abs(strwidth(legend, units = "user",
                                   cex = cex, font = text.font)))
  else if (!is.numeric(text.width) || text.width < 0)
    stop("'text.width' must be numeric, >= 0")
  xc <- Cex * xinch(cin[1L], warn.log = FALSE)
  yc <- Cex * yinch(cin[2L], warn.log = FALSE)
  if (xc < 0)
    text.width <- -text.width
  xchar <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
  ychar <- yextra + ymax
  if (trace)
    catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,
                                                   ychar))
  if (mfill) {
    xbox <- xc * 0.8
    ybox <- yc * 0.5
    dx.fill <- xbox
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty >
                                                            0))) || !missing(lwd)
  n.legpercol <- if (horiz) {
    if (ncol != 1)
      warning(gettextf("horizontal specification overrides: Number of columns := %d",
                       n.leg), domain = NA)
    ncol <- n.leg
    1
  }
  else ceiling(n.leg/ncol)
  has.pch <- !missing(pch) && length(pch) > 0
  if (do.lines) {
    x.off <- if (merge)
      -0.7
    else 0
  }
  else if (merge)
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  if (has.pch) {
    if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L],
                                                      type = "c") > 1) {
      if (length(pch) > 1)
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type = "c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    if (!is.character(pch))
      pch <- as.integer(pch)
  }
  if (is.na(auto)) {
    if (xlog)
      x <- log10(x)
    if (ylog)
      y <- log10(y)
  }
  if (nx == 2) {
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top <- y[2L]
    w <- diff(x)
    h <- diff(y)
    w0 <- w/ncol
    x <- mean(x)
    y <- mean(y)
    if (missing(xjust))
      xjust <- 0.5
    if (missing(yjust))
      yjust <- 0.5
  }
  else {
    h <- (n.legpercol + (!is.null(title))) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if (mfill)
      w0 <- w0 + dx.fill
    if (do.lines)
      w0 <- w0 + (seg.len + x.off) * xchar
    w <- ncol * w0 + 0.5 * xchar
    if (!is.null(title) && (abs(tw <- strwidth(title, units = "user",
                                               cex = cex) + 0.5 * xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    if (is.na(auto)) {
      left <- x - xjust * w
      top <- y + (1 - yjust) * h
    }
    else {
      usr <- par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L] * (usr[2L] - usr[1L])
      left <- switch(auto, bottomright = , topright = ,
                     right = usr[2L] - w - insetx, bottomleft = ,
                     left = , topleft = usr[1L] + insetx, bottom = ,
                     top = , center = (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L] * (usr[4L] - usr[3L])
      top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                      h + insety, topleft = , top = , topright = usr[4L] -
                      insety, left = , right = , center = (usr[3L] +
                                                             usr[4L] + h)/2)
    }
  }
  if (plot && bty != "n") {
    if (trace)
      catn("  rect2(", left, ",", top, ", w=", w, ", h=",
           h, ", ...)", sep = "")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL,
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1),
                                              rep.int(n.legpercol, ncol)))[1L:n.leg]
  yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol,
                                             ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
  if (mfill) {
    if (plot) {
      if (!is.null(fill))
        fill <- rep_len(fill, n.leg)
      rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox,
            col = fill, density = density, angle = angle,
            border = border)
    }
    xt <- xt + dx.fill
  }
  if (plot && (has.pch || do.lines)) {
    pt.COL <- rep_len(pt.col, n.leg)
    line.COL <- rep_len(line.col, n.leg)
  }
  if (missing(lwd) || is.null(lwd))
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty))
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) &
      !is.na(lwd)
    if (trace)
      catn("  segments2(", xt[ok.l] + x.off * xchar, ",",
           yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
    if (plot)
      segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len *
                  xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
                col = line.COL[ok.l])
    xt <- xt + (seg.len + x.off) * xchar
  }
  if (has.pch) {
    pch <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex <- rep_len(pt.cex, n.leg)
    pt.lwd <- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch)
    if (!is.character(pch)) {
      ok <- ok & (pch >= 0 | pch <= -32)
    }
    else {
      ok <- ok & nzchar(pch)
    }
    x1 <- (if (merge && do.lines)
      xt - (seg.len/2) * xchar
      else xt)[ok]
    y1 <- yt[ok]
    if (trace)
      catn("  points2(", x1, ",", y1, ", pch=", pch[ok],
           ", ...)")
    if (plot)
      points2(x1, y1, pch = pch[ok], col = pt.COL[ok], cex = pt.cex[ok],
              bg = pt.bg[ok], lwd = pt.lwd[ok])
  }
  xt <- xt + x.intersp * xchar
  if (plot) {
    if (!is.null(title))
      text2(left + w * title.adj, top - ymax, labels = title,
            adj = c(title.adj, 0), cex = cex, col = title.col)
    text2(xt, yt, labels = legend, adj = adj, cex = cex,
          col = text.col, font = text.font)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top),
                 text = list(x = xt, y = yt)))
}








jpeg("ADD FOLDER NAME HERE/file/FigureS6_NEW.jpg",width=6,height=8,pointsize=12,units="in",res=600)
#layout(matrix(c(1,1,2,2,2,2), ncol=2, nrow=3, byrow = TRUE))
par(mar=c(4,1.2,0.5,0.5))
plot(y=seq(nrow(AR_5km)),x=AR_5km$EST_F,pch=16,bty="n",xlab="% of prevalance explained",ylab="",yaxt="n",type="n",xlim=c(0,40),xaxt="n",cex.lab=0.9)
axis(1,seq(0,50,by=5),cex.axis=0.9)
abline(v=c(0,5,10,15,20,25,30,35,40,45,50),col="grey",lty=3)

for (aa in seq(7)) {
  if(aa==1) {ypos<-0}
  if(aa>=2) {
    ypos<-nrow(AR_5km_S[[aa-1]])+ypos
  }
  arrows(AR_5km_S[[aa]]$E95L_F,seq(nrow(AR_5km_S[[aa]]))+ypos,AR_5km_S[[aa]]$E95H_F,seq(nrow(AR_5km_S[[aa]]))+ypos,code=0,col=rgb(0.2+aa*0.07,0.7,0.5-aa*0.07,0.3,maxColorValue = 1))
  points(y=seq(nrow(AR_5km_S[[aa]]))+ypos,x=AR_5km_S[[aa]]$EST_F,
         pch=rep(rep(c(16,17),4)[aa],nrow(AR_5km_S[[aa]])),
         bty="n",col=rgb(1-aa*0.07,0.3,0.1+aa*0.07,1,maxColorValue = 1),cex=0.5)
}
LEGEND("topright",pt.col=rev(colormat),line.col=rev(colormat_CI),STATE_AF_5km$StateName,bty="n",pch=c(16,17,16,17,16,17,16),lwd=1,cex=1)
dev.off()


AR_5km$EST_CASES <- AR_5km$EST*AR_5km$POP/100

write.csv(AR_5km,"ADD FOLDER NAME HERE/file/AR_5km_NEW.csv")




