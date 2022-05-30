//! This crate contains tools related to error reporting in Lite. This is achieved through
//! the `LiteError` trait which can be implemented in different parts of the parser, representing
//! error types and information specific to that area of the parser. Anything that implements the
//! `LiteError` trait can then be used to construct a report using the `ariadne` crate.

#![warn(clippy::pedantic, clippy::nursery)]

use ariadne::{Color, Label, Report, ReportKind, Source};

/// This trait can be implemented to provide an interface usable for
/// creating reportss
pub trait LiteError {
    fn labels(&self) -> Vec<(String, std::ops::Range<usize>)>;
    fn message(&self) -> String;
    fn kind(&self) -> ReportKind;
    fn help(&self) -> Option<String> {
        None
    }
    fn note(&self) -> Option<String> {
        None
    }
    fn color(&self) -> Color {
        match self.kind() {
            ReportKind::Error => Color::Red,
            ReportKind::Warning => Color::Yellow,
            ReportKind::Advice => Color::Cyan,
            ReportKind::Custom(_, color) => color,
        }
    }
    fn build_report(&self) -> ariadne::Report {
        let offset = &self.labels().iter().fold(
            0,
            |acc, (_, span)| if span.start > acc { span.start } else { acc },
        );
        let mut report = Report::build(self.kind(), (), *offset).with_message(self.message());

        report.add_labels(
            self.labels()
                .iter()
                .map(|(label, span)| Label::new(span.start..span.end).with_message(label)),
        );

        if let Some(note) = self.note() {
            report.set_note(note);
        }

        if let Some(help) = self.help() {
            report.set_help(help);
        }

        report.finish()
    }
}

/// This struct holds a vec of errors to be reported by the parser
pub struct Reporter<'a> {
    reports: Vec<Box<dyn LiteError>>,
    source: &'a str,
}

impl<'a> Reporter<'a> {
    /// Constructs a new `Reporter` from a vec of errors and a source string
    ///
    /// # Examples
    ///
    /// ```rust
    /// let reporter = Reporter::new(vec![], "5 + 5");
    /// ```
    pub fn new(reports: Vec<Box<dyn LiteError>>, source: &'a str) -> Self {
        Self { reports, source }
    }

    /// Loops over all reports and prints them to the stderr
    pub fn report(&self) -> Result<(), std::io::Error> {
        for report in &self.reports {
            report.build_report().eprint(Source::from(self.source))?;
        }

        Ok(())
    }

    /// Adds a single report to the `Reporter`
    pub fn add_report(&mut self, report: Box<dyn LiteError>) {
        self.reports.push(report);
    }

    /// Extends the `Reporter` from a vec of reports
    pub fn add_reports(&mut self, mut reports: Vec<Box<dyn LiteError>>) {
        self.reports.append(&mut reports);
    }
}
