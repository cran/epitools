\name{oswego}
\docType{data}
\alias{oswego}
\title{Outbreak of Gastrointestinal Illness in Oswego County, 1940}
\description{
  On April 19, 1940, the local health officer in the village of
  Lycoming, Oswego County, New York, reported the occurrence of an
  outbreak of acute gastrointestinal illness to the District Health
  Officer in Syracuse. Dr. A. M. Rubin, epidemiologist-in-training, was
  assigned to conduct an investigation. 

  When Dr. Rubin arrived in the field, he learned from the health
  officer that all persons known to be ill had attended a church supper
  held on the previous evening, April 18. Family members who did not
  attend the church supper did not become ill. Accordingly, Dr. Rubin
  focused the investigation on the supper. He completed Interviews with
  75 of the 80 persons known to have attended, collecting information
  about the occurrence and time of onset of symptoms, and foods
  consumed. Of the 75 persons interviewed, 46 persons reported
  gastrointestinal illness.

  The onset of illness in all cases was acute, characterized chiefly by
  nausea, vomiting, diarrhea, and abdominal pain. None of the ill
  persons reported having an elevated temperature; all recovered within
  24 to 30 hours. Approximately 20% of the ill persons visited
  physicians. No fecal specimens were obtained for bacteriologic
  examination.

  The supper was held in the basement of the village church. Foods were
  contributed by numerous members of the congregation. The supper began
  at 6:00 p.m. and continued until 11:00 p.m. Food was spread out on a
  table and consumed over a period of several hours. Data regarding
  onset of illness and food eaten or water drunk by each of the 75
  persons interviewed are provided in the attached line listing (Oswego
  dataset). The approximate time of eating supper was collected for only
  about half the persons who had gastrointestinal illness.    
}
\usage{
##data(oswego)
}
\format{
  \itemize{
    \item{\code{id}}{
      subject identificaton number
    }
    \item{\code{age}}{
      age
    }
    \item{\code{sex}}{
      sex: F = Female, M = Male
    }
    \item{\code{meal.time}}{
      meal time on April 18th
    }
    \item{\code{ill}}{
      developed illness: Y = Yes N = No
    }
    \item{\code{onset.date}}{
      onset date: "4/18" = April 18th, "4/19" = April 19th
    }
    \item{\code{onset.time}}{
      onset time: HH:MM AM/PM
    }
    \item{\code{baked.ham}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{spinach}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{mashed.potato}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{cabbage.salad}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{jello rolls}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{brown.bread}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{milk}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{coffee}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{water}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{cakes}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{vanilla.ice.cream}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{chocolate.ice.cream}}{
      consumed item: Y = Yes N = No
    }
    \item{\code{fruit.salad}}{
      consumed item: Y = Yes N = No
    }
  }
}

\source{
  Center for Disease Control and Prevention, Epidemic Intelligence
  Service
}
\references{
  Oswego: An Outbreak of Gastrointestinal Illness Following a Church
  Supper (updated 2003):  S. aureus outbreak among church picnic
  attendees, 1940; the classic, straightforward outbreak investigation
  in a defined population. Training modules available at
  \url{https://www.cdc.gov/eis/casestudies/xoswego.401-303.student.pdf}.
}
\keyword{datasets}
