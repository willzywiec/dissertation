%% dissertation-gwu.cls
%%
%% This file provides a LaTeX class that is consistent with the guidelines provided
%% by The George Washington University's School of Engineering and Applied Science.
%%
%%  <http://library.gwu.edu/node/1128>
%%
%% This class is based on the LaTeX template provided at:
%%
%%  <http://www-personal.umich.edu/~dalle/codes/dissertation-umich/>
%%
%% The first line of the .tex file should be:
%%  \documentclass{dissertation-gwu}
%%  \documentclass{./tex/dissertation-gwu}

%% HEADERS ------------------------------------------------------------------------------------------------------------

% prevent the compiler from running on old versions of LaTeX
\NeedsTeXFormat{LaTeX2e}

% class name
\ProvidesClass{dissertation-gwu}[2019/11/25 v1.6.1 GWU dissertation template]

%% OPTIONS ------------------------------------------------------------------------------------------------------------

% define three switches for the the dissertation format types
% the variables are \@gwu@dissertation and \@gwu@report
% default values are FALSE
\newif\if@gwu@dissertation
\newif\if@gwu@debug % option for debugging
\newif\if@gwu@tableofcontents

\newif\if@gwu@listoffigures
\newif\if@gwu@listoftables
\newif\if@gwu@listofmaps
\newif\if@gwu@listofillustrations
\newif\if@gwu@listofprograms
\newif\if@gwu@listofabbrevs
\newif\if@gwu@listofsymbols
\newif\if@gwu@listofacronyms
\newif\if@gwu@listofglossariesabbrevs
\newif\if@gwu@listofglossariessymbols
\newif\if@gwu@listofglossariesacronyms
\newif\if@gwu@glossaryofterms

\newif\if@gwu@copyright
\newif\if@gwu@frontispiece
\newif\if@gwu@committeepage
\newif\if@gwu@dedication
\newif\if@gwu@acknowledgments
\newif\if@gwu@preface
\newif\if@gwu@prologue
\newif\if@gwu@foreword
\newif\if@gwu@nomenclature
\newif\if@gwu@abstract
\newif\if@gwu@msdegree
\newif\if@gwu@titlepage

% set the default value of some options to TRUE
\@gwu@dissertationtrue
% \@gwu@listoffigurestrue
% \@gwu@listoftablestrue
% \@gwu@copyrighttrue
% \@gwu@committeepagefalse
\@gwu@msdegreetrue

% set an index variable
% an index is not allowed in dissertations
\newif\if@gwu@index

% set bibliography backref
\newif\if@gwu@backref

% declare option for overall layout
\DeclareOption{dissertation}{\@gwu@dissertationtrue}

% if debug option is called then use the dissertation option but do not report
\DeclareOption{debug}{\@gwu@debugtrue\@gwu@dissertationtrue}

% declare option for index
\DeclareOption{index}{\@gwu@indextrue}

% declare option for bibliograph backref
\DeclareOption{backref}{\@gwu@backreftrue}

% pass any other options to report.cls
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}
\typeout{Unknown option: passing ``\CurrentOption"
option on to the standard LaTeX report class.}}

% this command tells LaTeX to process class options
\ProcessOptions

% load report.cls
% the base font size should be 10pt unless using the submit option
\LoadClass[letterpaper,12pt,oneside]{report}

% ensure index is off for dissertation
\if@gwu@dissertation
  \@gwu@indexfalse
\fi

%% LOAD PACKAGES ------------------------------------------------------------------------------------------------------

% use the geometry.sty package to set the margins
\if@gwu@debug
  % show margins 
  \RequirePackage[top=1in,bottom=1in,left=1.25in,right=1.25in,showframe]{geometry}
\else
  % use standard margins
  \RequirePackage[top=1in,bottom=1in,left=1.25in,right=1.25in]{geometry}
\fi

% check for obsolete packages
% \RequirePackage[l2tabu,orthodox]{nag}

% load math packages
\RequirePackage[binary-units=true]{siunitx}
\RequirePackage{amsmath}
\RequirePackage{amsfonts}
\RequirePackage{amssymb}
\RequirePackage{mathtools}
\RequirePackage{amsthm}

% load graphics package
\RequirePackage{graphicx}

% improve text spacing
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{lmodern}
\RequirePackage{microtype}
\RequirePackage{bm}

% improve font (Times New Roman)
\RequirePackage{mathptmx}

% load package for managing section headings
\RequirePackage[]{titlesec}

% load subcaption package (useful for complex figures)
\RequirePackage{subcaption}

% compress multiple citations
% \RequirePackage{cite}

% this package is used to generate a 'code' environment
\RequirePackage{verbatim}

% load upquote package (useful for quotes)
% \RequirePackage{upquote}

% this package enables the use of proper single quotes in text
% \RequirePackage{textcomp}

% this package is used to set line spacing
\RequirePackage{setspace}

% this package is used to create tables that can span multiple pages
% \RequirePackage{longtable}

% the package enables conditional testing of variables
\RequirePackage{ifthen}

% this package is used for silly letter spacing
\RequirePackage{soul}

% this package is used to generate alternate environments
\RequirePackage{float}

% this package is used for making an index
\if@gwu@index
  \RequirePackage{makeidx}
  \makeindex
\fi

% this package enables special headers and footers
\RequirePackage{fancyhdr}

% some extra packages to help with debugging
\if@gwu@debug
  \RequirePackage[final]{showlabels} show labels for referencing
  \RequirePackage{refcheck} check for unused references/labels
\fi

%% HYPERREF -----------------------------------------------------------------------------------------------------------

% this package enables extra colors for links
\RequirePackage[]{color}
% custom color for references
% \definecolor{DarkGreen}{rgb}{0,0.6,0}

% this makes labels and reference hyperlinks
\if@gwu@backref
  % use references in the bibliography
  \RequirePackage[pagebackref=true]{hyperref}
\else
  % do not use back references
  \RequirePackage{hyperref}
\fi
  \hypersetup{
    unicode=false,       % non-Latin characters in Acrobat’s bookmarks
    pdftoolbar=true,     % show Acrobat’s toolbar?
    pdfmenubar=true,     % show Acrobat’s menu?
    pdffitwindow=false,  % fit window to page when opened
    pdfstartview={FitV}, % fits the width of the page to the window
    pdfnewwindow=true,   % links in new PDF window
    colorlinks=false,    % boxed links: FALSE; colored links: TRUE
    bookmarksdepth=3,
    bookmarksopen=true,
  }

% cleveref referencing (must come after hyperref)
\AtEndOfClass{\RequirePackage[noabbrev,capitalize]{cleveref}}

% glossaries package (must come after hyperref)
% this package is used for the list of abbreviations
\RequirePackage[printonlyused]{acronym}
\RequirePackage[abbreviations,automake,nogroupskip,shortcuts=none,symbols]{glossaries-extra} % added nogroupskip
%
%% FORMATTING ---------------------------------------------------------------------------------------------------------
%
% set page style to fancy
\pagestyle{fancy}
% this command runs at the beginning of each chapter
% it puts the name of the chapter into the 'leftmark' parameter
\renewcommand{\chaptermark}[1]{\markboth{#1}{}}
% this command runs at the beginning of each section
% it puts the number and name of the section into the 'rightmark' parameter
\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}}

% clear the current header and footer
\fancyhf{}

% put the page number at the bottom-center of each page
\fancyfoot[C]{\thepage}
% get rid of the line at the top of each page
\renewcommand{\headrulewidth}{0pt}

% set headers and footers for less-marked pages
\fancypagestyle{plain}{
% get rid of the headers on plain pages
  \fancyhead{}
  \renewcommand{\headrulewidth}{0pt}
  \fancyfoot[C]{\thepage}
}

%% PAGE STYLES --------------------------------------------------------------------------------------------------------

% \ttlpg is the same as \titlepage in report.sty
% it is used here to set the low-level style of front pages
\newcommand*{\ttlpg}{
  % temporarily set one-column switch to FALSE
  \@restonecolfalse
  % check the number of columns
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \newpage
  \fi
  % remove headers and footers on title page
  \thispagestyle{empty}
  % set the page counter
  \c@page \z@
}

% same idea as \ttlpg above but with page numbers
\newcommand*{\frntpg}{
  % temporarily set one-column switch to FALSE
  \@restonecolfalse
  % check the number of columns
  \if@twocolumn
    \@restonecoltrue\onecolumn
  \else
    \newpage
  \fi
  % regular headers and footers
  \thispagestyle{plain}
}

%% GLOSSARY STYLES ----------------------------------------------------------------------------------------------------

% define a new glossary style for acronyms and symbols
\renewcommand*{\glossarysection}[2][]{
  {\centering\bfseries{#2}\par}
  \phantomsection\addcontentsline{toc}{chapter}{#1}
}

\newglossarystyle{acronymsandsymbols}{
  \setglossarystyle{list}
  \renewcommand*{\glossarypreamble}{\vspace{4ex}}
  \renewcommand*{\glossaryheader}{}
}

\newglossarystyle{glossaryofterms}{
  \setglossarystyle{list}
  \renewcommand*{\glossarypreamble}{\vspace{4ex}}
  \renewcommand*{\glossaryheader}{}
}

%% TITLE PAGE ---------------------------------------------------------------------------------------------------------

% title page fields
\renewcommand{\@title}{Insert a Title!}
\renewcommand{\@author}{Insert an Author!}

\newcommand{\@bsdepartment}{Insert a BS department name!}
\newcommand{\@bsschool}{Insert BS school!}
\newcommand{\@bsgrad}{BS Grad date!}

\newcommand{\@msdepartment}{Insert a MS department name!}
\newcommand{\@msschool}{Insert MS school!}
\newcommand{\@msgrad}{MS Grad date!}

\newcommand{\@degree}{Doctor of Philosophy}
\newcommand{\@department}{Insert a Department Name!}
\newcommand{\@committee}{Insert a Committee!}
\newcommand{\@chair}{Insert a Chair!}
\newcommand{\@chairtitle}{Professor of INSERT Title!}
\newcommand{\@cochair}{Insert a Co-Chair!}
\newcommand{\@cochairtitle}{Professor of INSERT Title!}
\newcommand{\@phdgrad}{PhD Grad date!}
\newcommand{\@defensedate}{Insert defense date!}

% commands to set title page fields
\renewcommand{\title}[1]{\renewcommand{\@title}{#1}\@gwu@titlepagetrue}
\renewcommand{\author}[1]{\renewcommand{\@author}{#1}}

\newcommand{\bsdepartment}[1]{\renewcommand{\@bsdepartment}{#1}}
\newcommand{\bsschool}[1]{\renewcommand{\@bsschool}{#1}}
\newcommand{\bsgrad}[1]{\renewcommand{\@bsgrad}{#1}}

\newcommand{\showmsdegree}{\@gwu@msdegreetrue}
\newcommand{\hidemsdegree}{\@gwu@msdegreefalse}

\newcommand{\msdepartment}[1]{\renewcommand{\@msdepartment}{#1}}
\newcommand{\msschool}[1]{\renewcommand{\@msschool}{#1}}
\newcommand{\msgrad}[1]{\renewcommand{\@msgrad}{#1}}

\newcommand{\degree}[1]{\renewcommand{\@degree}{#1}}
\newcommand{\department}[1]{\renewcommand{\@department}{#1}}
\newcommand{\committee}[1]{\renewcommand{\@committee}{#1}\@gwu@committeepagetrue}
\newcommand{\chair}[1]{\renewcommand{\@chair}{#1}}
\newcommand{\chairtitle}[1]{\renewcommand{\@chairtitle}{#1}}
\newcommand{\cochair}[1]{\renewcommand{\@cochair}{#1}}
\newcommand{\cochairtitle}[1]{\renewcommand{\@cochairtitle}{#1}}
\newcommand{\phdgrad}[1]{\renewcommand{\@phdgrad}{#1}}
\newcommand{\defensedate}[1]{\renewcommand{\@defensedate}{#1}}

% commands to use defined fields
\newcommand{\inserttitle}{\@title}
\newcommand{\insertauthor}{\@author}

\newcommand{\insertbsdepartment}{\@bsdepartment}
\newcommand{\insertbsschool}{\@bsschool}
\newcommand{\insertbsgrad}{\@bsgrad}

\newcommand{\insertmsdepartment}{\@msdepartment}
\newcommand{\insertmsschool}{\@msschool}
\newcommand{\insertmsgrad}{\@msgrad}

\newcommand{\insertdegree}{\@degree}
\newcommand{\insertdepartment}{\@department}
\newcommand{\insertcommittee}{\@committee}
\newcommand{\insertchair}{\@chair}
\newcommand{\insertchairtitle}{\@chairtitle}
\newcommand{\insertcochair}{\@cochair}
\newcommand{\insertcochairtitle}{\@cochairtitle}
\newcommand{\insertyear}{\number\year}
\newcommand{\insertphdgrad}{\@phdgrad}

\newcommand{\insertdefensedate}{\@defensedate}
% this redefines the title page to automatically have all of the features and properties that it should according to the guidelines
\renewcommand*{\titlepage}{
 % use title page formatting
 \ttlpg
 % use single-spaced lines
 \begin{singlespace}
  % move the page down slightly
  \hbox{\vspace{0.3in}}
  % center the title page
  \begin{center}
   % use 1.5 spacing
   \begin{singlespacing}
    % insert title
    {\normalsize\bfseries\inserttitle}
    \end{singlespacing} \\[3\baselineskip]
   % vertical rubber spacing
   % \vfill
   % print the word 'by'
   by \insertauthor \\[3\baselineskip]
   % more rubber spacing
   % \vfill
   % previous degrees
   \if@gwu@msdegree
    BS in \insertbsdepartment, \insertbsgrad, \insertbsschool \\
    MSE in \insertmsdepartment, \insertmsgrad, \insertmsschool \\[3\baselineskip]
   \else
    BS in \insertbsdepartment, \insertbsgrad, \insertbsschool \\[3\baselineskip]
   \fi
   % text from guidelines
   A Dissertation submitted to \\[3\baselineskip]
   The Faculty of \\
   The School of Engineering and Applied Science \\
   of The George Washington University \\
   in partial satisfaction of the requirements \\
   for the degree of \insertdegree \\[3\baselineskip]
   \insertphdgrad \\[3\baselineskip]
   % insert doctoral advisors
   Dissertation directed by \\[3\baselineskip]
   \insertchair \\
   \insertchairtitle \\[2ex]
   % and \\[2ex]
   \insertcochair \\
   \insertcochairtitle
  \end{center}
 \end{singlespace}
}

%% FRONTISPIECE -------------------------------------------------------------------------------------------------------

% command for the frontispiece
\newcommand{\@frontispiece}{\@empty}

% command to set the frontispiece
\newcommand{\frontispiece}[1]{
 % turn the frontispiece on
 \@gwu@frontispiecetrue
 % set the frontispiece
 \renewcommand{\@frontispiece}{#1}}

% command to insert the frontispiece
\newcommand{\insertfrontispiece}{\@frontispiece}

% command for the frontispiece
\newcommand*{\frontispiecepage}{
 % use titlepage formatting
 \ttlpg
 % center the frontispiece
 \begin{center}
  % vertical rubber spacing
  \ \vfill
  % insert the frontispiece
  \insertfrontispiece
  % vertical rubber spacing
  \vfill
 \end{center}
}

%% DISSERTATION COMMITTEE PAGE ----------------------------------------------------------------------------------------

% command for the committee page 
\newcommand*{\committeepage}{
  % use front page formatting
  \frntpg
    \begin{doublespace}
    \noindent
    The School of Engineering and Applied Science of The George Washington
    University certifies that \insertauthor~has passed the Final Examination for the
    degree of \insertdegree~as of \insertdefensedate. This is the
    final and approved form of the dissertation.
    \end{doublespace}
  % center the committee page
  \begin{center}
    \begin{singlespace}
      {\normalsize\bfseries\inserttitle} \\[2ex]
      \insertauthor \\[3\baselineskip]
    \end{singlespace}
\end{center}
  % non-justified content
  \begin{flushleft}
   % heading for the committee page
   Dissertation Research Committee: \\[4ex]
   % horizontal spacing
   \hspace{2em}
   % insert the committee page
   \parbox{0.9\textwidth}{\insertcommittee}
  \end{flushleft}
  % vertical rubber spacing
  \vfill
}

%% COPYRIGHT PAGE -----------------------------------------------------------------------------------------------------

% command for the name of the copyright holder
\newcommand{\@copyright}{\@author}

% command to change the copyright holder
\newcommand{\copyrightholder}[1]{\renewcommand{\@copyright}{#1}}

% command to insert the copyright holder
\newcommand{\insertcopyrightholder}{\@copyright}

% command for the copyright page
\newcommand*{\copyrightpage}{
 % use title page formatting
 \frntpg
 % center the copyright page
 \begin{center}
  % vertical rubber spacing
  \ \vfill
  % insert the frontispiece
  \copyright~Copyright~\number\year~by~\insertcopyrightholder \\
  All rights reserved
  % horizontal line
  % \rule[0.5ex]{5in}{1pt} \\[1ex]
  % vertical rubber spacing
  \vfill
 \end{center}
}

%% STYLE FOR REMAINING FRONT PAGES ------------------------------------------------------------------------------------

% command to set the spacing for all-caps headings
\sodef\an{}{.6em}{1em plus1em}{2em plus.1em minus.1em}

% default command
\newcommand{\@frontpagestyle}{2}
% command to change the default command
\newcommand{\frontpagestyle}[1]{\renewcommand{\@frontpagestyle}{#1}}

% command to insert front page style
\newcommand{\insertfront}[4]{
 % check the value of the front page style
 \ifthenelse{\equal{#2}{1}}{
  % style 1: plain text
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the front page
  \begin{center}
   % make a mini page
   \begin{minipage}{#4\textwidth}
    % use 1.5 spacing
    \if@gwu@dissertation
     \onehalfspacing
    \fi
    \sloppy #1
   \end{minipage}
  \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{2}}
 }{
 \ifthenelse{\equal{#2}{2}}{
  % style 2: bold heading at top of page, double spacing for text
  % center the front page
   % heading
   \begin{center}\textbf{\normalsize{#3}}\end{center}
   \begin{doublespace}
    \par #1
   \end{doublespace}
  % vertical rubber spacing
  \vspace*{\stretch{1}}
 }{
 \ifthenelse{\equal{#2}{3}}{
  % style 3: spread caps heading
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the page
  \begin{center}
   % make a mini page
   \begin{minipage}{#4\textwidth}
    % use 1.5 spacing
    \if@gwu@dissertation
     \onehalfspacing
    \fi
    % heading
    \begin{center} \textbf{\uppercase{\an{#3}}} \end{center}
    \sloppy #1
   \end{minipage}
  \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{2}}
 }{
 \ifthenelse{\equal{#2}{4}}{
  % style 4: lined
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the front page
  \begin{center}
   % make a mini page
   \begin{minipage}{#4\textwidth}
    % insert line
    \rule[1.4ex]{\textwidth}{1pt}
    % use 1.5 spacing
    \if@gwu@dissertation
     \onehalfspacing
    \fi
    \sloppy #1 \\
    % insert line
    \rule{\textwidth}{1pt}
   \end{minipage}
  \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{2}}
 }{
 \ifthenelse{\equal{#2}{5}}{
  % style 5: bold italic heading with lines
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the front page
  \begin{center}
    % make a mini page
   \begin{minipage}{#4\textwidth}
    % Heading
    \begin{center} \textbf{\textit{#3}} \end{center}
    % move up a bit
    \\\[-4ex]
    % insert line
    \rule[1.4ex]{\textwidth}{1pt}
    % use 1.5 spacing
    \if@gwu@dissertation
     \onehalfspacing
    \fi
    \sloppy #1 \\
    \rule{\textwidth}{1pt}
   \end{minipage}
  \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{2}}
 }{
 \ifthenelse{\equal{#2}{6}}{
  % style 6: lined with spread caps header
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the front page
  \begin{center}
    % make a mini page
   \begin{minipage}{#4\textwidth}
    % heading
    \begin{center} \textbf{\uppercase{\an{#3}}} \end{center}
    % move up a bit
    \\\[-4ex]
    % insert line
    \rule[1.4ex]{\textwidth}{1pt}
    % use 1.5 spacing
    \if@gwu@dissertation
     \onehalfspacing
    \fi
    \sloppy #1 \\
    % insert line
    \rule{\textwidth}{1pt}
   \end{minipage}
  \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{2}}
 }{
  % vertical rubber spacing
  \vspace*{\stretch{1}}
  % center the front page
  \begin{center}
   % make a mini page
   \begin{minipage}{0.7\textwidth}
    \begin{center} \sloppy #1 \end{center}
   \end{minipage}
 \end{center}
  % vertical rubber spacing
  \vspace*{\stretch{1}}
 }}}}}}
}

%% COMMANDS TO SHOW/HIDE FRONT PAGES ----------------------------------------------------------------------------------

% commands to show front pages
\newcommand{\showtitlepage}{\@gwu@titlepagetrue}
\newcommand{\showcopyright}{\@gwu@copyrighttrue}
\newcommand{\showdedication}{\@gwu@dedicationtrue}
\newcommand{\showacknowledgments}{\@gwu@acknowledgmentstrue}
\newcommand{\showpreface}{\@gwu@prefacetrue}
\newcommand{\showforeword}{\@gwu@forewordtrue}
\newcommand{\showprologue}{\@gwu@prologuetrue}
\newcommand{\shownomenclature}{\@gwu@nomenclaturetrue}
\newcommand{\showabstract}{\@gwu@abstracttrue}
\newcommand{\showcommitteepage}{\@gwu@committeepagetrue}

% commands to hide front pages
\newcommand{\hidetitlepage}{\@gwu@titlepagefalse}
\newcommand{\hidecopyright}{\@gwu@copyrightfalse}
\newcommand{\hidededication}{\@gwu@dedicationfalse}
\newcommand{\hideacknowledgments}{\@gwu@acknowledgmentsfalse}
\newcommand{\hidepreface}{\@gwu@prefacefalse}
\newcommand{\hideforeword}{\@gwu@forewordfalse}
\newcommand{\hideprologue}{\@gwu@prologuefalse}
\newcommand{\hidenomenclature}{\@gwu@nomenclaturefalse}
\newcommand{\hideabstract}{\@gwu@abstractfalse}
\newcommand{\hidecommitteepage}{\@gwu@committeepagefalse}

%% DEDICATION ---------------------------------------------------------------------------------------------------------

\newcommand{\@dedication}{}
% command to set the dedication style
\newcommand{\@dedicationstyle}{\@frontpagestyle}
% command to set the width of the dedication text area
\newcommand{\@dedication@width}{1}

% command to change dedication default
\newcommand{\dedication}[2][]{
 % show dedication
 \showdedication
 % set dedication text
 \renewcommand{\@dedication}{#2}
 % set dedication style
 \ifthenelse{\equal{#1}{}}{}{\renewcommand{\@dedicationstyle}{#1}}}
% command to change just the dedication style
\newcommand{\dedicationstyle}[1]{\renewcommand{\@dedicationstyle}{#1}}
% command to adjust the dedication text area width
\newcommand{\dedicationwidth}[1]{\renewcommand{\@dedication@width}{#1}}

% command to insert dedication
\newcommand{\insertdedication}{\@dedication}

% command for dedication
\newcommand*{\dedicationpage}{
 % set front page formatting
 \frntpg
 % add dedication to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Dedication}
 % insert dedication text and formatting
 \insertfront{\@dedication
  }{\@dedicationstyle}{Dedication}{\@dedication@width}
}

%% ACKNOWLEDGMENTS ---------------------------------------------------------------------------------------------------

\newcommand{\@acknowledgments}{}
% command to set the acknowledgments style
\newcommand{\@acknowledgmentsstyle}{\@frontpagestyle}
% command to set the width of the acknowledgments text area
\newcommand{\@acknowledgments@width}{1}

% command to change acknowledgments default
\newcommand{\acknowledgments}[2][]{
 % show acknowledgments
 \showacknowledgments
 % set acknowledgments text
 \renewcommand{\@acknowledgments}{#2}
 % set acknowledgments style
 \ifthenelse{\equal{#1}{}}{}{
  \renewcommand{\@acknowledgmentsstyle}{#1}}}
% command to set the width of the acknowledgments text area
\newcommand{\acknowledgmentswidth}[1]{
 \renewcommand{\@acknowledgments@width}{#1}}
% command to change acknowledgments style
\newcommand{\acknowledgmentsstyle}[1]{
 \renewcommand{\@acknowledgmentsstyle}{#1}}

% command to insert acknowledgments
\newcommand{\insertacknowledgments}{\@acknowledgments}

% command for acknowledgments
\newcommand*{\acknowledgmentspage}{
 % set front page formatting
 \frntpg
 % add acknowledgments to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Acknowledgments}
 % insert acknowledgments text and formatting
 \insertfront{\@acknowledgments
  }{\@acknowledgmentsstyle}{Acknowledgments}{\@acknowledgments@width}
}

%% PREFACE ------------------------------------------------------------------------------------------------------------

\newcommand{\@preface}{}
% command to set the preface style
\newcommand{\@prefacestyle}{\@frontpagestyle}
% command to set the width of the preface text area
\newcommand{\@preface@width}{1}

% command to change preface default
\newcommand{\preface}[2][]{
 % show preface
 \showpreface
 % set preface text
 \renewcommand{\@preface}{#2}
 % set preface style
 \ifthenelse{\equal{#1}{}}{}{
  \renewcommand{\@prefacestyle}{#1}}}
% command to set the width of the preface text area
\newcommand{\prefacewidth}[1]{
 \renewcommand{\@preface@width}{#1}}
% command to change preface style
\newcommand{\prefacestyle}[1]{
 \renewcommand{\@prefacestyle}{#1}}

% command to insert preface
\newcommand{\insertpreface}{\@preface}

% command for preface
\newcommand*{\prefacepage}{
 % set front page formatting
 \frntpg
 % add preface to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Preface}
 % insert preface text and formatting
 \insertfront{\@preface
  }{\@prefacestyle}{Preface}{\@preface@width}
}

%% FOREWORD -----------------------------------------------------------------------------------------------------------

\newcommand{\@foreword}{}
% command to set the foreword style
\newcommand{\@forewordstyle}{\@frontpagestyle}
% command to set the width of the foreword text area
\newcommand{\@foreword@width}{1}

% command to change foreword default
\newcommand{\foreword}[2][]{
 % show foreword
 \showforeword
 % set foreword text
 \renewcommand{\@foreword}{#2}
 % set foreword style
 \ifthenelse{\equal{#1}{}}{}{
  \renewcommand{\@forewordstyle}{#1}}}
% command to set the width of the foreword text area
\newcommand{\forewordwidth}[1]{
 \renewcommand{\@foreword@width}{#1}}
% command to change foreword style
\newcommand{\forewordstyle}[1]{
 \renewcommand{\@forewordstyle}{#1}}

% command to insert foreword
\newcommand{\insertforeword}{\@foreword}

% command for foreword
\newcommand*{\forewordpage}{
 % set foreword formatting
 \frntpg
 % add foreword to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Foreword}
 % insert foreword text and formatting
 \insertfront{\@foreword
  }{\@forewordstyle}{Foreword}{\@foreword@width}
}

%% PROLOGUE -----------------------------------------------------------------------------------------------------------

\newcommand{\@prologue}{}
% command to set the prologue style
\newcommand{\@prologuestyle}{\@frontpagestyle}
% command to set the width of the prologue text area
\newcommand{\@prologue@width}{1}

% command to change prologue default
\newcommand{\prologue}[2][]{
 % show prologue
 \showprologue
 % set prologue text
 \renewcommand{\@prologue}{#2}
 % set prologue style
 \ifthenelse{\equal{#1}{}}{}{
  \renewcommand{\@prologuestyle}{#1}}}
% command to set the width of the prologue text area
\newcommand{\prologuewidth}[1]{
 \renewcommand{\@prologue@width}{#1}}
% command to change prologue style
\newcommand{\prologuestyle}[1]{
 \renewcommand{\@prologuestyle}{#1}}

% command to insert prologue
\newcommand{\insertprologue}{\@prologue}

% command for prologue
\newcommand*{\prologuepage}{
 % set prologue formatting
 \frntpg
 % add prologue to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Prologue}
 % insert prologue text and formatting
 \insertfront{\@prologue
  }{\@prologuestyle}{Prologue}{\@prologue@width}
}

%% LIST COMMANDS ------------------------------------------------------------------------------------------------------

% commands to show each of the lists
\newcommand{\showtableofcontents}{\@gwu@tableofcontentstrue}
\newcommand{\showlistoffigures}{\@gwu@listoffigurestrue\@gwu@tableofcontentstrue}
\newcommand{\showlistoftables}{\@gwu@listoftablestrue\@gwu@tableofcontentstrue}
\newcommand{\showlistofmaps}{\@gwu@listofmapstrue\@gwu@tableofcontentstrue}
\newcommand{\showlistofillustrations}{\@gwu@listillustrationstrue\@gwu@tableofcontentstrue}
\newcommand{\showlistofprograms}{\@gwu@listofprogramstrue\@gwu@tableofcontentstrue}
\newcommand{\showlistofabbreviations}{\@gwu@listofabbrevstrue\@gwu@tableofcontentstrue}
\newcommand{\showlistofacronyms}{\@gwu@listofacronymstrue\@gwu@listofglossariesabbrevsfalse\@gwu@tableofcontentstrue}
\newcommand{\showlistofsymbols}{\@gwu@listofsymbolstrue\@gwu@listofglossariessymbolsfalse\@gwu@tableofcontentstrue}

\newcommand{\showglossarieslistofabbreviations}{\@gwu@listofglossariesabbrevstrue\@gwu@listofacronymsfalse\@gwu@tableofcontentstrue}
\newcommand{\showglossarieslistofsymbols}{\@gwu@listofglossariessymbolstrue\@gwu@listofsymbolsfalse\@gwu@tableofcontentstrue}
\newcommand{\showglossarieslistofacronyms}{\@gwu@listofglossariesacronymstrue\@gwu@listofacronymsfalse\@gwu@tableofcontentstrue}
\newcommand{\showglossariesglossaryofterms}{\@gwu@glossaryoftermstrue\@gwu@tableofcontentstrue}

% commands to hide each of the lists
\newcommand{\hidetableofcontents}{\@gwu@tableofcontentsfalse}
\newcommand{\hidelistoffigures}{\@gwu@listoffiguresfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistoftables}{\@gwu@listoftablesfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofmaps}{\@gwu@listofmapsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofillustrations}{\@gwu@listillustrationsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofprograms}{\@gwu@listofprogramsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofabbreviations}{\@gwu@listofabbrevsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofacronyms}{\@gwu@listofacronymsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hidelistofsymbols}{\@gwu@listofsymbolsfalse\@gwu@tableofcontentsfalse}

\newcommand{\hideglossarieslistofabbreviations}{\@gwu@listofglossariesabbrevsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hideglossarieslistofacronyms}{\@gwu@listofglossariesacronymsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hideglossarieslistofsymbols}{\@gwu@listofglossariessymbolsfalse\@gwu@tableofcontentsfalse}
\newcommand{\hideglossariesglossaryofterms}{\@gwu@glossaryoftermsfalse\@gwu@tableofcontentsfalse}

%% TABLE OF CONTENTS --------------------------------------------------------------------------------------------------

% set the formatting for chapter entries in the table of contents
\renewcommand*\l@chapter[2]{
 % add some vertical space
 \vskip 1ex plus 1pt minus 1pt
 % format the spacing and use dots between title and number
 \@dottedtocline{1}{0em}{1.2em}
 % set the formatting for the chapter name and page number
 {\textbf{#1}}{\textbf{#2}}
 % add some vertical space
 \vskip 1ex}

% set the spacing for other table of contents entries
\renewcommand*{\l@section}{\@dottedtocline{2}{2.5em}{2em}}
\renewcommand*{\l@subsection}{\@dottedtocline{3}{5.0em}{2.8em}}
\renewcommand*{\l@subsubsection}{\@dottedtocline{4}{7.5em}{3.5em}}

% number of section layers to show in table of contents
\setcounter{tocdepth}{3}
% number of subsection layers to allow in document
\setcounter{secnumdepth}{3}

% set the formatting of the table of contents
\renewcommand*{\tableofcontents}{
 % move to a new page
 \newpage
 % add a PDF anchor
 \phantomsection
 % add a bookmark manually for the table of contents
 \pdfbookmark[0]{Table of Contents}{\thepage}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{Table of Contents}}\end{center}
 % add some space after the title
 \vspace{2ex}
 % start automatic table of contents features
 \begin{singlespace}
  \@starttoc{toc}\if@restonecol\twocolumn\fi
 \end{singlespace}
}

% adjust spacing for page numbers
\renewcommand{\@pnumwidth}{2.0em} % default is 1.55em

%% LIST OF FIGURES ----------------------------------------------------------------------------------------------------

% set the formatting for the list of figures
\renewcommand*{\l@figure}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of figures
\renewcommand*{\listoffigures}{
 % move to a new page
 \newpage
 % add list of figures to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Figures}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Figures}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \@starttoc{lof}\if@restonecol\twocolumn\fi
 % \end{singlespace}
}

%% LIST OF TABLES -----------------------------------------------------------------------------------------------------

% set the formatting for the list of tables
\renewcommand*{\l@table}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of tables
\renewcommand*{\listoftables}{
 % move to a new page
 \newpage
 % add list of tables to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Tables}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Tables}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % start automatic table of contents features
 % \begin{singlespace}
  \@starttoc{lot}\if@restonecol\twocolumn\fi
 % \end{singlespace}
}

%% LIST OF MAPS -------------------------------------------------------------------------------------------------------

% command to generate the 'map' environment
\newfloat{map}{tbp}{lom}[chapter]
% use a capitalized version for the captions
\floatname{map}{Map}

% set the formatting for the list of maps
\newcommand*{\l@map}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of maps
\newcommand*{\listofmaps}{
 % move to a new page
 \newpage
 % add list of maps to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Maps}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Maps}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \@starttoc{lom}\if@restonecol\twocolumn\fi
 % \end{singlespace}
}

%% LIST OF ILLUSTRATIONS ----------------------------------------------------------------------------------------------

% command to generate the 'illustration' environment
\newfloat{illustration}{tbp}{loi}[chapter]
% use a capitalized version for the captions
\floatname{illustration}{Illustration}

% set the formatting for the list of illustrations
\newcommand*{\l@illustration}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of illustrations
\newcommand*{\listofillustrations}{
 % move to a new page
 \newpage
 % add list of illustrations to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Illustrations}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Illustrations}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \@starttoc{loi}\if@restonecol\twocolumn\fi
 % \end{singlespace}
}

%% LIST OF PROGRAMS ---------------------------------------------------------------------------------------------------

% command to generate the 'list of programs' environment
\newfloat{program}{tbp}{lop}[chapter]
% use a capitalized version for the captions
\floatname{program}{Program}

% set the formatting for the list of programs
\newcommand*{\l@program}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of programs
\newcommand*{\listofprograms}{
 % move to a new page
 \newpage
 % add list of programs to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Programs}
 % add a PDF anchor
 \phantomsection
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Programs}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \@starttoc{lop}\if@restonecol\twocolumn\fi
 % \end{singlespace}
}

%% LIST OF ABBREVIATIONS ----------------------------------------------------------------------------------------------

% variable holding the contents of the list of abbreviations
\newcommand{\@abbrevs}{}

% command to set the list of abbreviations
\newcommand{\abbreviations}[1]{
 % show the list of abbreviations
 \@gwu@listofabbrevstrue
 % redefine the list of abbreviations
 \renewcommand{\@abbrevs}{#1}}

% set the formatting for the list of abbreviations
\newcommand*{\l@abbreviation}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of abbreviations
\newcommand*{\listofabbreviations}{
 % move to a new page
 \newpage
 % add list of abbreviations to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Abbreviations}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Abbreviations}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \begin{acronym}
   \@abbrevs
  \end{acronym}
 % \end{singlespace}
}


%% LIST OF ACRONYMS ---------------------------------------------------------------------------------------------------

% variable holding the contents of the list of acronyms
\newcommand{\@acronyms}{}

% command to set the list of acronyms
\newcommand{\acronyms}[1]{
 % show the list of acronyms
 \@gwu@listofacronymstrue
 % redefine the list of acronyms
 \renewcommand{\@acronyms}{#1}}

% set the formatting for the list of acronyms
\newcommand*{\l@acronym}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of acronyms
\newcommand*{\listofacronyms}{
 % move to a new page
 \newpage
 % add list of acronyms to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Acronyms}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Acronyms}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \begin{acronym}
   \@acronyms
  \end{acronym}
 % \end{singlespace}
}

%% LIST OF SYMBOLS ----------------------------------------------------------------------------------------------------

% variable holding the contents of the list of symbols
\newcommand{\@symbols}{}

% command to set the list of symbols
\newcommand{\symbols}[1]{
 % show the list of symbols
 \@gwu@listofsymbolstrue
 % redefine the list of symbols
 \renewcommand{\@symbols}{#1}}

% set the formatting for the list of symbols
\newcommand*{\l@symbol}{\@dottedtocline{1}{0em}{2.5em}}

% set the formatting for the list of symbols
\newcommand*{\listofsymbols}{
 % move to a new page
 \newpage
 % add list of symbols to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{List of Symbols}
 % use front page style
 \frntpg
 % add title
 \begin{center}{\normalsize\textbf{List of Symbols}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vspace{1ex}
 % start automatic table of contents features
 % \begin{singlespace}
  \begin{acronym}
   \@symbols
  \end{acronym}
 % \end{singlespace}
}

%% NOMENCLATURE -------------------------------------------------------------------------------------------------------

% new environment for nomenclature
\newenvironment{symboltable}[1][]
{
 % insert the title if present
 \ifthenelse{\equal{#1}{}}{
  % start the tabular environment
  \noindent\longtable[l]{@{} p{3.6em} @{\hspace{2pt}=\hspace{1em}} l}
 }{
 % insert title
 \noindent\textit{#1}\\[-4ex]
 % start the tabular environment
 \noindent\longtable[l]{@{} p{3.6em} @{\hspace{2pt}=\hspace{1em}} l}
 }
}{
 % end the tabular environment
 \endlongtable
}

% new command for nomenclature
\newcommand{\@nomenclature}{}

% command to set nomenclature
\newcommand{\nomenclature}[1]{
 % show nomenclature
 \@gwu@nomenclaturetrue
 % redefine nomenclature
 \renewcommand{\@nomenclature}{#1}}

% command to insert nomenclature
\newcommand{\insertnomenclature}{
 % move to a new page
 \newpage
 % add nomenclature to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Nomenclature}
% use front page style
 \frntpg
 % add the title
 \begin{center}{\normalsize\textbf{Nomenclature}}\\[2\baselineskip] \end{center}%
 % add some space after the title
 % \vskip4ex
% set the line skip
\begin{onehalfspace}
% insert nomenclature contents
\@nomenclature
\end{onehalfspace}
}

%% LIST OF GLOSSARY ABBREVIATIONS -------------------------------------------------------------------------------------

% set the formatting of the list of glossary abbreviations
\newcommand*{\listofglossaryabbreviations}{
    % \newpage
 % use front page style
 \frntpg
 % start the automatic table of contents features
 \printglossary[type=\glsxtrabbrvtype,style=acronymsandsymbols,title={\normalsize List of Abbreviations}]
}


% set the formatting of the list of glossary abbreviations
\newcommand*{\listofglossarysymbols}{
    % \newpage
 % use front page style
 \frntpg
 % start the automatic list of glossary abbreviations feature
 \printglossary[type=symbols,style=acronymsandsymbols,title={\normalsize List of Symbols}]
}

\newcommand*{\listofglossaryofterms}{
\frntpg
\printglossary[style=glossaryofterms,type=main,title={\normalsize Glossary of Terms}]
}

%% ABSTRACT -----------------------------------------------------------------------------------------------------------

\newcommand{\@abstract}{}

% command to set abstract
\renewcommand{\abstract}[1]{
 % show the abstract
 \showabstract
 \renewcommand{\@abstract}{#1}}

% command to insert abstract
\newcommand{\insertabstract}{\@abstract}

% check abstract page number
\newif\if@abstract@pageno
\if@gwu@dissertation
 \@abstract@pagenotrue
\fi

% command to show abstract page number
\newcommand{\showbstractpagenumber}{
 \@abstract@pagenotrue}
% command to hide abstract page number
\newcommand{\hideabstractpagenumber}{
 \@abstract@pagenofalse}

% format abstract
\newcommand{\abstractpage}{
 % set abstract heading
 \frntpg
 \if@abstract@pageno
 \else
  \thispagestyle{empty}
 \fi
 % add abstract to the table of contents
 \phantomsection\addcontentsline{toc}{chapter}{Abstract} 
 \begin{center}
  {\normalsize \textbf{Abstract}} \\[5ex]
  {\normalsize \bfseries \inserttitle} \\[2ex]
 \end{center}
 \begin{doublespace}
  \sloppy
  \par \insertabstract
 \end{doublespace}
 % vertical rubber spacing
 \vspace*{\stretch{1}}
}

%% BIBLIOGRAPHY -------------------------------------------------------------------------------------------------------

% save the original bibliography command
\let\@tex@bibliography\bibliography

% change the bibliography header
\renewcommand*{\bibname}{\centerline{\normalsize Bibliography}}

% create a new command for the bibliography
\renewcommand*{\bibliography}[1]{
  % update bibliography format
  % switch to single spacing
  \singlespacing

  % move to a new page
  \clearpage
  % add a PDF anchor
  \phantomsection
  % add bibliography to table of contents
  \addcontentsline{toc}{chapter}{Bibliography}
  % load the bibliography
  \@tex@bibliography{#1}
}

%% CHAPTER/SECTION HEADINGS -------------------------------------------------------------------------------------------

% change the chapter headings to upper case format
% \renewcommand{\@chapapp}{CHAPTER}

% change the formatting of the first page of each chapter
% \renewcommand{\@makechapterhead}[1]{
% insert an extra top margin
% \begin{minipage}{0.8\textwidth} \vspace{1in} \end{minipage} \\
%  {
% ensure proper alignment
%   \parindent 0pt \raggedright
% check for chapter overflow
%   \ifnum \c@secnumdepth >\m@ne
% chapter and number
%   \begin{center}{\Large\textbf{ \@chapapp{} \thechapter }} \end{center} \par
% vertical spacing
%   \vskip 0.3in \fi
% insert chapter title
%   \begin{center} \LARGE \textbf{ #1} \end{center}
% insert vertical space after chapter title
%   \nobreak \vskip 0.3in
%  }
% }

\titleformat{\chapter}[hang]
{\normalfont\normalsize\bfseries\centering}{\chaptertitlename\ \thechapter:}{20pt}{\normalsize}
\titleformat{\section}
{\normalfont\normalsize\bfseries}{\thesection}{1em}{\normalsize}
\titleformat{\subsection}
{\normalfont\normalsize\bfseries}{\thesubsection}{1em}{\normalsize}
\titleformat{\subsubsection}
{\normalfont\normalsize\bfseries}{\thesubsubsection}{1em}{}
\titleformat{\paragraph}[runin]
{\normalfont\normalsize\bfseries}{\theparagraph}{1em}{}
\titleformat{\subparagraph}[runin]
{\normalfont\normalsize\bfseries}{\thesubparagraph}{1em}{}

\titlespacing*{\chapter} {0pt}{*0}{*4}
\titlespacing*{\section} {0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex}
\titlespacing*{\subsection} {0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}
\titlespacing*{\subsubsection}{0pt}{3.25ex plus 1ex minus .2ex}{1.5ex plus .2ex}
\titlespacing*{\paragraph} {0pt}{3.25ex plus 1ex minus .2ex}{1em}
\titlespacing*{\subparagraph} {\parindent}{3.25ex plus 1ex minus .2ex}{1em}

%% APPENDIX -----------------------------------------------------------------------------------------------------------

%% SPACING ------------------------------------------------------------------------------------------------------------

% fix the spacing below each caption
\setlength\belowcaptionskip{7pt plus 4pt minus 1pt}

%% INITIALIZATION -----------------------------------------------------------------------------------------------------

% commands to be placed after \begin{document}
\AtBeginDocument{
    \providecommand\tikzifexternalizing[2]{#2}
% insert title page
\tikzifexternalizing{}{\if@gwu@titlepage\titlepage\fi
  % change the PDF title
  \hypersetup{
  pdftitle=\inserttitle,
  pdfauthor=\insertauthor,
  pdfcreator=\insertauthor,
  pdfproducer=\insertauthor}
  \if@gwu@committeepage\committeepage\fi
  % initialize page numbers
  \setcounter{page}{2}
  % insert page numbers at the bottom of each page
  \pagestyle{plain}
  % use lowercase Roman numerals
  \renewcommand{\thepage}{\roman{page}}
  \if@gwu@copyright\copyrightpage\fi
  \if@gwu@dedication\dedicationpage\fi
  \if@gwu@acknowledgments\acknowledgmentspage\fi
  \if@gwu@frontispiece\frontispiecepage\fi
  % \ifthenelse{\equal{\@frontispiece}{}}{}{\frontispiecepage}
  \if@gwu@abstract\abstractpage\fi
  \if@gwu@tableofcontents\tableofcontents\fi
  \if@gwu@listoffigures\listoffigures\fi
  \if@gwu@listoftables\listoftables\fi
  \if@gwu@listofmaps\listofmaps\fi
  \if@gwu@listofillustrations\listofillustrations\fi
  \if@gwu@listofprograms\listofprograms\fi
  \if@gwu@listofabbrevs\listofabbreviations\fi
  \if@gwu@listofglossariesabbrevs\listofglossaryabbreviations\fi
  \if@gwu@listofacronyms\listofacronyms\fi
  \if@gwu@listofsymbols\listofsymbols\fi
  \if@gwu@listofglossariessymbols\listofglossarysymbols\fi
  \if@gwu@nomenclature\insertnomenclature\fi
  \if@gwu@glossaryofterms\listofglossaryofterms\fi
  \if@gwu@preface\prefacepage\fi
  \if@gwu@foreword\forewordpage\fi
  \if@gwu@prologue\prologuepage\fi
  % move to a new page
  \newpage
  % restart page numbers for each chapter
  \setcounter{page}{1}
  % insert page numbers at the bottom of each page
  \pagestyle{plain}
  % use Arabic numerals
  \renewcommand{\thepage}{\arabic{page}}
  % add the word 'Chapter' to the table of contents
  % \addtocontents{toc}{\vspace{1.3ex}\noindent
  % {\bfseries Chapter}\vspace{1.3ex}}
  % use the correct formatting
  \if@gwu@dissertation
    % use 1.5 spacing
    \doublespacing
  \else
    % use two-column format
    \twocolumn
  \fi
  }
}
