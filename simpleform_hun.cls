\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{simpleform}[2009/11/20 v1.0 A Simple Form]

\LoadClass[12pt,a4paper]{article}

\RequirePackage{wasysym}

\RequirePackage[utf8]{inputenc}
\pagestyle{empty}

\newcommand*{\@fillinfont}{\rmfamily}
\newcommand*{\@formfont}{\sffamily}
\newcommand*{\@unchecked}{\Square}
\newcommand*{\@checked}{\XBox}

\newcommand*{\@forenames}{}
\newcommand*{\forenames}[1]{\renewcommand*{\@forenames}{#1}}

\newcommand*{\@surname}{}
\newcommand*{\surname}[1]{\renewcommand*{\@surname}{#1}}

\newcommand*{\@address}{\rule{0pt}{1in}}
\newcommand*{\address}[1]{\renewcommand*{\@address}{#1}}

\newcommand*{\@comments}{\rule{0pt}{1in}}
\newcommand*{\comments}[1]{\renewcommand*{\@comments}{#1}}

\newcommand*{\reset@gender}{%
  \def\gender@male{\@unchecked}%
  \def\gender@female{\@unchecked}%
}

\reset@gender

\newcommand*{\gender}[1]{%
  \@ifundefined{gender@#1}%
  {%
    \ClassError{simpleform}{Unknown gender `#1'}%
      {Gender can either be `male' or `female'}%
  }%
  {%
    \reset@gender
    \@namedef{gender@#1}{\@checked}%
  }%
}

\newcommand*{\@validages}{%
  {<18}{\textless18},%
  {18-30}{18--30},%
  {31-40}{31--40},%
  {41-60}{41--60},%
  {>60}{\textgreater60}%
}

\newcommand*{\get@age}[2]{%
  \def\@age@label{#1}%
  \def\@age@textt{#2}%
}

\newcommand*{\reset@ages}{%
  \@for\@age:=\@validages\do{%
    \expandafter\get@age\@age
    \@namedef{age@\@age@label}{\@unchecked}%
  }%
}

\reset@ages

\newcommand*{\age}[1]{%
  \@ifundefined{age@#1}%
  {%
    \def\@ageoptions{}%
    \def\@ageoptionssep{}%
    \@for\@age:=\@validages\do{%
       \expandafter\get@age\@age
      \edef\@ageoptions{\@ageoptions\@ageoptionssep\@age@label}%
      \def\@ageoptionssep{, }%
    }%
    \ClassError{simpleform}{Unknown age range `#1'}%
      {Available options: \@ageoptions}%
  }%
  {%
    \reset@ages
    \@namedef{age@#1}{\@checked}%
  }%
}

\newlength\boxwidth
\newlength\underlinewidth

\newcommand{\makeform}{%
  \@formfont
  \begin{center}\large\bfseries
  Kérdőív
  \end{center}
  \noindent
  {\renewcommand{\arraystretch}{1.5}%
    \settowidth{\underlinewidth}{Forenames}%
    \addtolength{\underlinewidth}{-\linewidth}%
    \addtolength{\underlinewidth}{2\tabcolsep}%
    \setlength{\underlinewidth}{-\underlinewidth}%
    \begin{tabular}{@{}ll@{}}
    Vezetéknév & \@fillinfont
      \underline{\makebox[\underlinewidth][l]{\@forenames}}\\
    Keresztnév & \@fillinfont
      \underline{\makebox[\underlinewidth][l]{\@surname}}\\
    Nem & férfi\ \gender@male\qquad nő \gender@female\\
    \end{tabular}%
  }%
  \par
  \vspace{\baselineskip}%
  \noindent
  Age range:\qquad \@for\@age:=\@validages\do{%
    \expandafter\get@age\@age
    \@age@textt\ \@nameuse{age@\@age@label}\qquad
    }%
  \par
  \vspace{\baselineskip}%
  \noindent
  Cím:\par
  \vspace{\baselineskip}%
  \noindent
  {\@fillinfont
    \begin{tabular}{@{}l@{}}
    \@address
    \end{tabular}%
  }%
  \par
  \vspace{\baselineskip}%
  \noindent
  Comments:\par
  \vspace{\baselineskip}%
  \noindent
  \setlength{\boxwidth}{\textwidth}%
  \addtolength{\boxwidth}{-2\fboxsep}%
  \addtolength{\boxwidth}{-2\fboxrule}%
  \fbox{\parbox{\boxwidth}{\@fillinfont\@comments}}
}
\endinput