use iced::*;
use std::f32::consts::{FRAC_PI_2, PI};

fn main() -> iced::Result {
    App::run(Settings::default())
}

pub struct App {
    canvas: Canvas,
}
impl App {
    pub fn new() -> Self {
        let crash_rotation = 2.7357345;
        App {
            canvas: Canvas::new("Test".to_string(), crash_rotation),
        }
    }
}
impl Application for App {
    type Executor = iced::executor::Default;
    type Message = ();
    type Flags = ();

    fn new(_flags: Self::Flags) -> (Self, Command<Self::Message>) {
        (App::new(), Command::none())
    }

    fn title(&self) -> String {
        "Test".to_string()
    }

    fn update(
        &mut self,
        _message: Self::Message,
        _clipboard: &mut Clipboard,
    ) -> Command<Self::Message> {
        Command::none()
    }

    fn view(&mut self) -> Element<'_, Self::Message> {
        let App { canvas } = self;
        canvas::Canvas::new(canvas)
            .width(Length::Fill)
            .height(Length::Fill)
            .width(Length::Fill)
            .height(Length::Fill)
            .into()
    }
}

pub struct Canvas {
    kreuzung: Kreuzung,
    rotation: f32,
    grab: Option<Vector>,
    position: Vector,
    cache: canvas::Cache,
}
impl Canvas {
    pub fn new(beschreibung: String, rotation: f32) -> Self {
        // 5207: L180mm, 24.28°, R437.4mm
        Canvas {
            kreuzung: Kreuzung {
                laenge: 180.,
                radius: 437.4,
                variante: Variante::MitKurve,
                beschreibung,
            },
            rotation,
            grab: None,
            position: Vector::new(0., 0.),
            cache: canvas::Cache::new(),
        }
    }
}
impl<T> canvas::Program<T> for Canvas {
    fn draw(&self, bounds: Rectangle, _cursor: canvas::Cursor) -> Vec<canvas::Geometry> {
        // crash passiert beim Füllen einer gedrehten Kreuzung (5207) während dem Verschieben nach unten???
        // debug-winkel 2.7357345
        // sollte `2.*PI - self.angle()` sein
        // TODO verwende Kreuzung-Geometrie
        vec![self.cache.draw(bounds.size(), |frame| {
            let mut center: Vector = bounds.size().into();
            center.x /= 2.;
            center.y /= 2.;
            let final_position = center + self.position;
            let a = if self.grab.is_some() { 0.5 } else { 1. };
            frame.with_save(|frame| {
                frame.translate(final_position);
                frame.rotate(self.rotation);
                // Fülle Hintergrund
                for (transformations, path) in self.kreuzung.fuelle() {
                    frame.with_save(|frame| {
                        for transformation in transformations {
                            transformation.apply_to(frame)
                        }
                        frame.fill(
                            &path,
                            canvas::Fill {
                                color: Color::from_rgba(1., 0., 0., a),
                                rule: canvas::FillRule::EvenOdd,
                                ..Default::default()
                            },
                        );
                    });
                }
            })
        })]
    }

    fn update(
        &mut self,
        event: canvas::Event,
        bounds: Rectangle,
        cursor: canvas::Cursor,
    ) -> (canvas::event::Status, Option<T>) {
        use canvas::event::Status::*;
        let event_status = match event {
            canvas::Event::Mouse(mouse::Event::ButtonPressed(iced::mouse::Button::Left))
                if cursor.is_over(&bounds) && self.rotation > FRAC_PI_2 =>
            {
                self.grab = cursor.position_in(&bounds).map(|p| Vector::new(p.x, p.y));
                canvas::event::Status::Captured
            }
            canvas::Event::Mouse(mouse::Event::ButtonReleased(iced::mouse::Button::Left))
                if self.grab.is_some() && self.rotation > FRAC_PI_2 =>
            {
                self.grab = None;
                canvas::event::Status::Captured
            }
            canvas::Event::Mouse(mouse::Event::CursorMoved { position: _ })
                if cursor.is_over(&bounds) && self.rotation > FRAC_PI_2 =>
            {
                if let Some(last_pos) = self.grab {
                    if let Some(in_pos) = cursor.position_in(&bounds) {
                        let in_vec = Vector::new(in_pos.x, in_pos.y);
                        let movement = in_vec - last_pos;
                        self.position = self.position + movement;
                        self.grab = Some(in_vec);
                        canvas::event::Status::Captured
                    } else {
                        canvas::event::Status::Ignored
                    }
                } else {
                    canvas::event::Status::Ignored
                }
            }
            _ => canvas::event::Status::Ignored,
        };
        if event_status == Captured {
            self.cache.clear();
        }
        (event_status, None)
    }
}

mod background {
    pub(crate) struct White;
    impl iced::container::StyleSheet for White {
        fn style(&self) -> iced::container::Style {
            iced::container::Style {
                background: Some(iced::Background::Color(iced::Color::WHITE)),
                ..Default::default()
            }
        }
    }
    pub(crate) struct Black;
    impl iced::container::StyleSheet for Black {
        fn style(&self) -> iced::container::Style {
            iced::container::Style {
                background: Some(iced::Background::Color(iced::Color::BLACK)),
                ..Default::default()
            }
        }
    }
}

pub const SPURWEITE: f32 = 16.5;
pub fn abstand() -> f32 {
    SPURWEITE / 3.0
}
pub fn beschraenkung() -> f32 {
    SPURWEITE + 2. * abstand()
}
pub fn radius_beschraenkung_aussen(radius: f32) -> f32 {
    radius + 0.5 * SPURWEITE + abstand()
}
pub fn radius_beschraenkung_innen(radius: f32) -> f32 {
    radius - 0.5 * SPURWEITE - abstand()
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Transformation {
    /// Verschiebe alle Koordinaten um den übergebenen Vector.
    Translate(Vector),
    /// Rotiere alle Koordinaten um den Ursprung (im Uhrzeigersinn)
    Rotate(f32),
    /// Skaliere alle Koordinaten (x',y') = (x*scale, y*scale)
    Scale(f32),
}
impl Transformation {
    pub fn apply_to(self, frame: &mut canvas::Frame) {
        match self {
            Transformation::Translate(vector) => frame.translate(vector),
            Transformation::Rotate(angle) => frame.rotate(angle),
            Transformation::Scale(scale) => frame.scale(scale),
        }
    }
}

mod gerade {
    use super::*;

    pub fn fuelle(
        laenge: f32,
        transformations: Vec<Transformation>,
        with_invert_y: bool,
    ) -> (Vec<Transformation>, canvas::Path) {
        let mut path_builder = canvas::path::Builder::new();
        // Koordinaten
        let gleis_links = 0.;
        let gleis_rechts = gleis_links + laenge;
        let beschraenkung_oben = 0.;
        let gleis_oben = beschraenkung_oben + abstand();
        let gleis_unten = gleis_oben + SPURWEITE;
        // invertertiere y, falls gefragt
        let multiplier = if with_invert_y { -1. } else { 1. };
        // Zeichne Umriss
        path_builder.move_to(Point::new(gleis_links, multiplier * gleis_oben));
        path_builder.line_to(Point::new(gleis_links, multiplier * gleis_unten));
        path_builder.line_to(Point::new(gleis_rechts, multiplier * gleis_unten));
        path_builder.line_to(Point::new(gleis_rechts, multiplier * gleis_oben));
        path_builder.line_to(Point::new(gleis_links, multiplier * gleis_oben));
        // Rückgabewert
        (transformations, path_builder.build())
    }
}

mod kurve {
    use super::*;
    pub fn size(radius: f32, winkel: f32) -> Size {
        // Breite
        let radius_beschraenkung_aussen = radius_beschraenkung_aussen(radius);
        let width_factor = if winkel.abs() < FRAC_PI_2 {
            winkel.sin()
        } else {
            1.
        };
        let width = radius_beschraenkung_aussen * width_factor;
        // Höhe des Bogen
        let angle_abs = winkel.abs();
        let comparison = if angle_abs < FRAC_PI_2 {
            radius_beschraenkung_aussen * (1. - winkel.cos()) + beschraenkung() * winkel.cos()
        } else if angle_abs < PI {
            radius_beschraenkung_aussen * (1. - winkel.cos())
        } else {
            radius_beschraenkung_aussen
        };
        // Mindesthöhe: Beschränkung einer Geraden
        let height = beschraenkung().max(comparison);
        // Rückgabewert
        Size { width, height }
    }

    pub fn fuelle(
        radius: f32,
        winkel: f32,
        transformations: Vec<Transformation>,
        with_invert_y: bool,
    ) -> (Vec<Transformation>, canvas::Path) {
        let mut path_builder = canvas::path::Builder::new();
        let spurweite = SPURWEITE;
        // Koordinaten für den Bogen
        let winkel_anfang = 3. * PI / 2.;
        let winkel_ende = winkel_anfang + winkel;
        let radius_innen = radius - 0.5 * spurweite;
        let radius_aussen = radius + 0.5 * spurweite;
        let bogen_zentrum_y = abstand() + radius_aussen;
        // invertertiere y, falls gefragt
        let multiplier = if with_invert_y { -1. } else { 1. };
        // Koordinaten links
        let gleis_links = 0.;
        let beschraenkung_oben = 0.;
        let gleis_links_oben = beschraenkung_oben + abstand();
        let gleis_links_unten = gleis_links_oben + spurweite;
        // Koordinaten rechts
        let gleis_rechts_oben = Point::new(
            gleis_links + radius_aussen * winkel.sin(),
            multiplier * (gleis_links_oben + radius_aussen * (1. - winkel.cos())),
        );
        let gleis_rechts_unten = Point::new(
            gleis_rechts_oben.x - spurweite * winkel.sin(),
            gleis_rechts_oben.y + multiplier * spurweite * winkel.cos(),
        );
        // obere Kurve
        path_builder.arc(canvas::path::Arc {
            center: Point::new(gleis_links, multiplier * bogen_zentrum_y),
            radius: radius_aussen,
            start_angle: multiplier * winkel_anfang,
            end_angle: multiplier * winkel_ende,
        });
        path_builder.close();
        // untere Kurve
        path_builder.arc(canvas::path::Arc {
            center: Point::new(gleis_links, multiplier * bogen_zentrum_y),
            radius: radius_innen,
            start_angle: multiplier * winkel_anfang,
            end_angle: multiplier * winkel_ende,
        });
        path_builder.close();
        // Zwischen-Teil
        path_builder.move_to(Point::new(gleis_links, multiplier * gleis_links_oben));
        path_builder.line_to(gleis_rechts_oben);
        path_builder.line_to(gleis_rechts_unten);
        path_builder.line_to(Point::new(gleis_links, multiplier * gleis_links_unten));
        path_builder.close();
        // Rückgabewert
        (transformations, path_builder.build())
    }
}

/// Definition einer Kreuzung
#[derive(Clone, Debug)]
pub struct Kreuzung {
    pub laenge: f32,
    pub radius: f32,
    pub variante: Variante,
    pub beschreibung: String,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variante {
    MitKurve,
    OhneKurve,
}
impl Kreuzung {
    pub(crate) fn angle(&self) -> f32 {
        // angle solves the formula `x = L/2 * (1 + sin(alpha)) = R * cos(alpha)`
        // https://www.wolframalpha.com/input/?i=sin%28alpha%29-C*cos%28alpha%29%3DC
        // length=0 gives angle=0, but is not properly defined,
        // since it violates the formula above (pi/2 required)
        // pi/2 doesn't work either, since it violates the formula
        // `y = L/2 * sin(alpha) = R * (1 - cos(alpha))`
        // only for radius=0 as well both formulas are satisfied by any angle
        2. * (0.5 * (self.laenge / self.radius)).atan()
    }

    pub(crate) fn size(&self) -> Size {
        let size_kurve = kurve::size(self.radius, self.angle());
        let height_beschraenkung = beschraenkung();
        let height_kurven = 2. * size_kurve.height - height_beschraenkung;
        Size::new(
            self.laenge.max(size_kurve.width),
            height_beschraenkung.max(height_kurven),
        )
    }

    pub(crate) fn fuelle(&self) -> Vec<(Vec<Transformation>, canvas::Path)> {
        // utility sizes
        let Size { width, height } = self.size();
        let half_width = 0.5 * width;
        let start_x = 0.;
        let half_height = 0.5 * height;
        let start_y = half_height - 0.5 * beschraenkung();
        let angle = self.angle();
        let mut paths = Vec::new();
        // Geraden
        let horizontal_transformations =
            vec![Transformation::Translate(Vector::new(start_x, start_y))];
        let gedreht_transformations = vec![
            Transformation::Translate(Vector::new(half_width, half_height)),
            Transformation::Rotate(angle),
            // transformations with assumed inverted y-Axis
            Transformation::Translate(Vector::new(-half_width, half_height)),
            Transformation::Translate(Vector::new(start_x, -start_y)),
        ];
        paths.push(gerade::fuelle(
            self.laenge,
            horizontal_transformations.clone(),
            false,
        ));
        paths.push(gerade::fuelle(
            self.laenge,
            gedreht_transformations.clone(),
            true,
        ));
        // Kurven
        if self.variante == Variante::MitKurve {
            paths.push(kurve::fuelle(
                self.radius,
                angle,
                horizontal_transformations,
                false,
            ));
            paths.push(kurve::fuelle(
                self.radius,
                angle,
                gedreht_transformations,
                true,
            ));
        }
        // return value
        paths
    }
}
