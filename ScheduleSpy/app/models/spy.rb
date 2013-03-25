class Spy < ActiveRecord::Base
  DEPARTMENTS = {
    'AANL - Ancient Anatolian Languages'                                     => 'AANL',
    'AFAM - African-American Studies'                                        => 'AFAM',
    'AKKD - Akkadian'                                                        => 'AKKD',
    'ANCC - Anesthesia and Critical Care'                                    => 'ANCC',
    'ANCM - Ancient Mediterranean World'                                     => 'ANCM',
    'ANTH - Anthropology'                                                    => 'ANTH',
    'ARAB - Arabic'                                                          => 'ARAB',
    'ARAM - Aramaic'                                                         => 'ARAM',
    'ARME - Armenian'                                                        => 'ARME',
    'ARTH - Art History'                                                     => 'ARTH',
    'ARTV - Visual Arts'                                                     => 'ARTV',
    'ASLG - American Sign Languages'                                         => 'ASLG',
    'ASTR - Astronomy and Astrophysics'                                      => 'ASTR',
    'BANG - Bangla'                                                          => 'BANG',
    'BCMB - Biochemistry and Molecular Biology'                              => 'BCMB',
    'BCSN - Bosnian, Croatian, and Serbian'                                  => 'BCSN',
    'BIBL - Biblical Studies'                                                => 'BIBL',
    'BIOS - Biological Sciences'                                             => 'BIOS',
    'BPHS - Biophysical Sciences'                                            => 'BPHS',
    'BPRO - Big Problems'                                                    => 'BPRO',
    'BSDG - Biological Sciences (Divisional Courses)'                        => 'BSDG',
    'BULG - Bulgarian'                                                       => 'BULG',
    'BUSE - Business (Evening Program)'                                      => 'BUSE',
    'BUSF - Business (Full-Time Program)'                                    => 'BUSF',
    'BUSL - Business (Executive MBA London Program)'                         => 'BUSL',
    'BUSS - Business (Executive MBA Singapore Program)'                      => 'BUSS',
    'BUSW - Business (Weekend Program)'                                      => 'BUSW',
    'BUSX - Business (Executive MBA Chicago Program)'                        => 'BUSX',
    'CABI - Cancer Biology'                                                  => 'CABI',
    'CATA - Catalan'                                                         => 'CATA',
    'CCTS - Committee on Clinical and Translational Science'                 => 'CCTS',
    'CDIN - Center for Disciplinary Innovation'                              => 'CDIN',
    'CHDV - Comparative Human Development'                                   => 'CHDV',
    'CHEM - Chemistry'                                                       => 'CHEM',
    'CHIN - Chinese'                                                         => 'CHIN',
    'CHRM - Committee on the Ministry'                                       => 'CHRM',
    'CHSS - Conceptual and Historical Studies of Science'                    => 'CHSS',
    'CLAS - Classics'                                                        => 'CLAS',
    'CLCV - Classical Civilization'                                          => 'CLCV',
    'CMLT - Comparative Literature'                                          => 'CMLT',
    'CMSC - Computer Science'                                                => 'CMSC',
    'CMST - Cinema and Media Studies'                                        => 'CMST',
    'COVA - Committee on Visual Arts'                                        => 'COVA',
    'CPHY - Cell Physiology'                                                 => 'CPHY',
    'CPNS - Computational Neuroscience'                                      => 'CPNS',
    'CRES - Comparative Race and Ethnic Studies'                             => 'CRES',
    'CRWR - Creative Writing'                                                => 'CRWR',
    'CSPP - Computer Science Professional Program'                           => 'CSPP',
    'CZEC - Czech'                                                           => 'CZEC',
    'DVBI - Developmental Biology'                                           => 'DVBI',
    'DVPR - Philosophy of Religions'                                         => 'DVPR',
    'DVSC - Special Courses in Divinity'                                     => 'DVSC',
    'EALC - East Asian Languages and Civilizations'                          => 'EALC',
    'ECEV - Ecology and Evolution'                                           => 'ECEV',
    'ECON - Economics'                                                       => 'ECON',
    'EEUR - East European'                                                   => 'EEUR',
    'EGPT - Egyptian'                                                        => 'EGPT',
    'EMED - Emergency Medicine'                                              => 'EMED',
    'ENGL - English Language and Literature'                                 => 'ENGL',
    'ENST - Environmental Studies'                                           => 'ENST',
    'EVOL - Evolutionary Biology'                                            => 'EVOL',
    'EXAM - Language Reading Exam'                                           => 'EXAM',
    'FINM - Financial Mathematics'                                           => 'FINM',
    'FMED - Family Medicine'                                                 => 'FMED',
    'FNDL - Fundamentals: Issues and Texts'                                  => 'FNDL',
    'FREN - French'                                                          => 'FREN',
    'GENE - Genetics'                                                        => 'GENE',
    'GEOG - Geographical Studies'                                            => 'GEOG',
    'GEOS - Geophysical Sciences'                                            => 'GEOS',
    'GNDR - Gender Studies'                                                  => 'GNDR',
    'GREK - Greek'                                                           => 'GREK',
    'GRMN - German'                                                          => 'GRMN',
    'HCHR - History of Christianity'                                         => 'HCHR',
    'HDCP - Human Development Clinical Program'                              => 'HDCP',
    'HEBR - Hebrew'                                                          => 'HEBR',
    'HGEN - Human Genetics'                                                  => 'HGEN',
    'HIJD - History of Judaism'                                              => 'HIJD',
    'HIND - Hindi'                                                           => 'HIND',
    'HIPS - History, Philosophy, and Social Studies of Science and Medicine' => 'HIPS',
    'HIST - History'                                                         => 'HIST',
    'HMRT - Human Rights'                                                    => 'HMRT',
    'HREL - History of Religions'                                            => 'HREL',
    'HSTD - Health Studies'                                                  => 'HSTD',
    'HUMA - Humanities'                                                      => 'HUMA',
    'IMET - Ideas and Methods'                                               => 'IMET',
    'IMMU - Immunology'                                                      => 'IMMU',
    'INDO - Indo-European Linguistics'                                       => 'INDO',
    'INRE - International Relations'                                         => 'INRE',
    'INST - International Studies'                                           => 'INST',
    'ISLM - Islamic Studies'                                                 => 'ISLM',
    'ISTP - Interdisciplinary Scientist Training Program'                    => 'ISTP',
    'ITAL - Italian'                                                         => 'ITAL',
    'JAPN - Japanese'                                                        => 'JAPN',
    'JWSC - Jewish Studies'                                                  => 'JWSC',
    'KORE - Korean'                                                          => 'KORE',
    'LACS - Latin American and Caribbean Studies'                            => 'LACS',
    'LATN - Latin'                                                           => 'LATN',
    'LAWS - Laws'                                                            => 'LAWS',
    'LGLN - Languages in Linguistics'                                        => 'LGLN',
    'LING - Linguistics'                                                     => 'LING',
    'LLSO - Law, Letters, and Society'                                       => 'LLSO',
    'MALA - Malayalam'                                                       => 'MALA',
    'MAPH - Master of Arts Program in the Humanities'                        => 'MAPH',
    'MAPS - Master of Arts Program in the Social Sciences'                   => 'MAPS',
    'MARA - Marathi'                                                         => 'MARA',
    'MATH - Mathematics'                                                     => 'MATH',
    'MDVS - MDIV Special Courses'                                            => 'MDVS',
    'MEDC - Medicine'                                                        => 'MEDC',
    'MGCB - Molecular Genetics and Cell Biology'                             => 'MGCB',
    'MICR - Microbiology'                                                    => 'MICR',
    'MLAP - Master of Liberal Arts Program'                                  => 'MLAP',
    'MOMN - Molecular Metabolism and Nutrition'                              => 'MOMN',
    'MPHY - Medical Physics'                                                 => 'MPHY',
    'MPMM - Molecular Pathogenesis and Molecular Medicine'                   => 'MPMM',
    'MSTR - Master of Science in Threat and Response Management'             => 'MSTR',
    'MUSI - Music'                                                           => 'MUSI',
    'NCDV - New Collegiate Division Courses'                                 => 'NCDV',
    'NEAA - Near Eastern Art and Archaeology'                                => 'NEAA',
    'NEHC - Near Eastern History and Civilizations'                          => 'NEHC',
    'NOND - Graham School (Non-Degree)'                                      => 'NOND',
    'NORW - Norwegian'                                                       => 'NORW',
    'NTEC - New Testament and ECL'                                           => 'NTEC',
    'NTSC - Natural Science'                                                 => 'NTSC',
    'NURB - Neurobiology'                                                    => 'NURB',
    'NURL - Neurology'                                                       => 'NURL',
    'OBGY - Obstetrics and Gynecology'                                       => 'OBGY',
    'OPTH - Ophthalmology and Visual Sciences'                               => 'OPTH',
    'ORGB - Organismal Biology and Anatomy'                                  => 'ORGB',
    'PALI - Pali'                                                            => 'PALI',
    'PATH - Pathology'                                                       => 'PATH',
    'PBPL - Public Policy Studies'                                           => 'PBPL',
    'PEDS - Pediatrics'                                                      => 'PEDS',
    'PERS - Persian'                                                         => 'PERS',
    'PHED - Physical Education'                                              => 'PHED',
    'PHIL - Philosophy'                                                      => 'PHIL',
    'PHSC - Physical Science'                                                => 'PHSC',
    'PHYS - Physics'                                                         => 'PHYS',
    'PLSC - Political Science'                                               => 'PLSC',
    'POLI - Polish'                                                          => 'POLI',
    'PORT - Portuguese'                                                      => 'PORT',
    'PPHA - Public Policy Studies'                                           => 'PPHA',
    'PRAC - SSA Field Practicum'                                             => 'PRAC',
    'PSCR - Psychiatry'                                                      => 'PSCR',
    'PSYC - Psychology'                                                      => 'PSYC',
    'RADI - Radiology'                                                       => 'RADI',
    'RCON - Radiation And Cellular Oncology'                                 => 'RCON',
    'REMS - Renaissance and Early Modern Studies'                            => 'REMS',
    'RETH - Religious Ethics'                                                => 'RETH',
    'RLIT - Religion and Literature'                                         => 'RLIT',
    'RLST - Religious Studies'                                               => 'RLST',
    'RUSS - Russian'                                                         => 'RUSS',
    'SALC - South Asian Languages and Civilizations'                         => 'SALC',
    'SANS - Sanskrit'                                                        => 'SANS',
    'SCTH - Social Thought'                                                  => 'SCTH',
    'SLAV - General Slavic'                                                  => 'SLAV',
    'SOCI - Sociology'                                                       => 'SOCI',
    'SOSC - Social Sciences'                                                 => 'SOSC',
    'SOSL - South Slavic'                                                    => 'SOSL',
    'SPAN - Spanish'                                                         => 'SPAN',
    'SSAD - Social Service Administration'                                   => 'SSAD',
    'STAT - Statistics'                                                      => 'STAT',
    'SURG - Surgery'                                                         => 'SURG',
    'SWAH - Swahili'                                                         => 'SWAH',
    'TAML - Tamil'                                                           => 'TAML',
    'TAPS - Theater and Performance Studies'                                 => 'TAPS',
    'TBTN - Tibetan'                                                         => 'TBTN',
    'THEO - Theology'                                                        => 'THEO',
    'TLGU - Telegu'                                                          => 'TLGU',
    'TTIC - Toyota Technological Institute at Chicago'                       => 'TTIC',
    'TURK - Turkish'                                                         => 'TURK',
    'UGAR - Ugaritic'                                                        => 'UGAR',
    'URDU - Urdu'                                                            => 'URDU',
    'UTEP - Urban Teacher Education Program (MAT)'                           => 'UTEP',
    'UZBK - Uzbek'                                                           => 'UZBK',
    'YDDH - Yiddish'                                                         => 'YDDH'
  }

  after_create :update_status
  validates :department, :inclusion => { :in => DEPARTMENTS.values } # :format => { :with => /^\w{4}$/i }
  validates :course,     :format    => { :with => /^\d{3,5}$/ }
  validates :section,    :format    => { :with => /^\w{2}$/ }
  validates :email,      :format    => { :with => /^.+?@uchicago\.edu$/ }


  scope :working, where( :mission_complete => false )

  def mail_or_not_to_mail(status)
    unless status == :working
      SpyMailer.send(status, self).deliver
    end
  end

  def update_status
    status = :working
    class_id  = "%s%s%s" % [ self.department, self.course, self.section ]
    class_url = "http://timeschedules.uchicago.edu/view.php?dept=#{self.department}#{TERM_ARGUMENT}"
    document  = Nokogiri::HTML( open( class_url ) )
    matches   = document.xpath( '//span[@class="smallredt"]/ancestor::a/ancestor::td/ancestor::tr' )
    enrolled = maximum = nil
    if matches.length <= 0
      status = :does_not_exist
    else
      matches.each do |class_match|
        if class_match.xpath( './td/a/span[@class="smallredt"]' ).text.gsub( /\W/, '' ) == class_id
          enrolled = class_match.xpath( './td/span' )[7].text.gsub( /\W/, '' ).to_i
          maximum  = class_match.xpath( './td/span' )[8].text.gsub( /\W/, '' )
        end
      end
      case maximum
      when /consentrequired/i
        status = :requires_consent
      when /nolimit/i
        status = :no_limit
      else
        if maximum.nil?
          status = :does_not_exist
        else
          maximum = maximum.to_i

          if enrolled < maximum
            status = :found_opening
          end
        end
      end
    end

    unless status == :working
      self.update_attribute :mission_complete, true
    else
      self.touch
    end

    self.mail_or_not_to_mail(status)
  end
end