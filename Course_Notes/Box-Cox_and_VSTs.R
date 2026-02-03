\documentclass{beamer}

\usetheme{Madrid}
\usecolortheme{default}

\title[Box--Cox vs VST]{Box--Cox Transformations vs\\Variance-Stabilizing Transformations}
\author{}
\date{}

\begin{document}

%-------------------------------------------------
  \begin{frame}
\titlepage
\end{frame}

%-------------------------------------------------
  \begin{frame}{Motivation}
\begin{itemize}
\item Linear regression assumes:
  \begin{itemize}
\item Linearity in the mean
\item Constant variance
\item Approximately normal errors
\end{itemize}
\item Transformations are often used when these assumptions fail
\item Two common approaches:
  \begin{itemize}
\item Variance-stabilizing transformations (VSTs)
\item Box--Cox transformations
\end{itemize}
\end{itemize}
\end{frame}

%-------------------------------------------------
  \begin{frame}{Variance-Stabilizing Transformations (VSTs)}
\begin{itemize}
\item Derived from an assumed mean--variance relationship
\[
  \mathrm{Var}(Y \mid \mu) = g(\mu)
  \]
\item Choose transformation \(h(Y)\) so that
\[
  \mathrm{Var}(h(Y)) \approx \text{constant}
  \]
\item Rooted in probability theory
\item Closely tied to the data-generating distribution
\end{itemize}
\end{frame}

%-------------------------------------------------
  \begin{frame}{Common Variance-Stabilizing Transformations}
\begin{center}
\begin{tabular}{l c}
\textbf{Distribution} & \textbf{VST} \\
\hline
Poisson counts & \(\sqrt{Y}\) \\
Binomial proportions & \(\arcsin(\sqrt{Y})\) \\
Gamma / multiplicative errors & \(\log Y\) \\
Normal (constant variance) & Identity \\
\end{tabular}
\end{center}
\end{frame}

%-------------------------------------------------
  \begin{frame}{When to Use a VST}
\begin{itemize}
\item Data-generating distribution is known or well-motivated
\item Variance clearly depends on the mean
\item Interpretation linked to the scientific process
\item Examples:
  \begin{itemize}
\item Event counts
\item Rates and intensities
\item Proportions and probabilities
\end{itemize}
\item Often preferable to modeling the distribution directly via a GLM
\end{itemize}
\end{frame}

%-------------------------------------------------
  \begin{frame}{Box--Cox Transformation}
\[
  Y^{(\lambda)} =
    \begin{cases}
  