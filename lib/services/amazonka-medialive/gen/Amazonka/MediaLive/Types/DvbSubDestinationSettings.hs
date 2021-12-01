{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.DvbSubDestinationSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.DvbSubDestinationSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.DvbSubDestinationAlignment
import Amazonka.MediaLive.Types.DvbSubDestinationBackgroundColor
import Amazonka.MediaLive.Types.DvbSubDestinationFontColor
import Amazonka.MediaLive.Types.DvbSubDestinationOutlineColor
import Amazonka.MediaLive.Types.DvbSubDestinationShadowColor
import Amazonka.MediaLive.Types.DvbSubDestinationTeletextGridControl
import Amazonka.MediaLive.Types.InputLocation
import qualified Amazonka.Prelude as Prelude

-- | Dvb Sub Destination Settings
--
-- /See:/ 'newDvbSubDestinationSettings' smart constructor.
data DvbSubDestinationSettings = DvbSubDestinationSettings'
  { -- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
    -- transparent. Leaving this parameter blank is equivalent to setting it to
    -- 0 (transparent). All burn-in and DVB-Sub font settings must match.
    backgroundOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
    -- transparent. All burn-in and DVB-Sub font settings must match.
    fontOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical offset of the shadow relative to the captions in
    -- pixels. A value of -2 would result in a shadow offset 2 pixels above the
    -- text. All burn-in and DVB-Sub font settings must match.
    shadowYOffset :: Prelude.Maybe Prelude.Int,
    -- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
    -- and DVB-Sub font settings must match.
    fontResolution :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the vertical position of the caption relative to the top of
    -- the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the top of the output. If no explicit yPosition
    -- is provided, the caption will be positioned towards the bottom of the
    -- output. This option is not valid for source captions that are STL,
    -- 608\/embedded or teletext. These source settings are already pre-defined
    -- by the caption stream. All burn-in and DVB-Sub font settings must match.
    yPosition :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the color of the rectangle behind the captions. All burn-in
    -- and DVB-Sub font settings must match.
    backgroundColor :: Prelude.Maybe DvbSubDestinationBackgroundColor,
    -- | Specifies the horizontal offset of the shadow relative to the captions
    -- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
    -- left. All burn-in and DVB-Sub font settings must match.
    shadowXOffset :: Prelude.Maybe Prelude.Int,
    -- | When set to auto fontSize will scale depending on the size of the
    -- output. Giving a positive integer will specify the exact font size in
    -- points. All burn-in and DVB-Sub font settings must match.
    fontSize :: Prelude.Maybe Prelude.Text,
    -- | Specifies the horizontal position of the caption relative to the left
    -- side of the output in pixels. A value of 10 would result in the captions
    -- starting 10 pixels from the left of the output. If no explicit xPosition
    -- is provided, the horizontal caption position will be determined by the
    -- alignment parameter. This option is not valid for source captions that
    -- are STL, 608\/embedded or teletext. These source settings are already
    -- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
    -- must match.
    xPosition :: Prelude.Maybe Prelude.Natural,
    -- | If no explicit xPosition or yPosition is provided, setting alignment to
    -- centered will place the captions at the bottom center of the output.
    -- Similarly, setting a left alignment will align captions to the bottom
    -- left of the output. If x and y positions are given in conjunction with
    -- the alignment parameter, the font will be justified (either left or
    -- centered) relative to those coordinates. Selecting \"smart\"
    -- justification will left-justify live subtitles and center-justify
    -- pre-recorded subtitles. This option is not valid for source captions
    -- that are STL or 608\/embedded. These source settings are already
    -- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
    -- must match.
    alignment :: Prelude.Maybe DvbSubDestinationAlignment,
    -- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
    -- Leaving this parameter blank is equivalent to setting it to 0
    -- (transparent). All burn-in and DVB-Sub font settings must match.
    shadowOpacity :: Prelude.Maybe Prelude.Natural,
    -- | Controls whether a fixed grid size will be used to generate the output
    -- subtitles bitmap. Only applicable for Teletext inputs and
    -- DVB-Sub\/Burn-in outputs.
    teletextGridControl :: Prelude.Maybe DvbSubDestinationTeletextGridControl,
    -- | Specifies font outline color. This option is not valid for source
    -- captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineColor :: Prelude.Maybe DvbSubDestinationOutlineColor,
    -- | Specifies font outline size in pixels. This option is not valid for
    -- source captions that are either 608\/embedded or teletext. These source
    -- settings are already pre-defined by the caption stream. All burn-in and
    -- DVB-Sub font settings must match.
    outlineSize :: Prelude.Maybe Prelude.Natural,
    -- | External font file used for caption burn-in. File extension must be
    -- \'ttf\' or \'tte\'. Although the user can select output fonts for many
    -- different types of input captions, embedded, STL and teletext sources
    -- use a strict grid system. Using external fonts with these caption
    -- sources could cause unexpected display of proportional fonts. All
    -- burn-in and DVB-Sub font settings must match.
    font :: Prelude.Maybe InputLocation,
    -- | Specifies the color of the shadow cast by the captions. All burn-in and
    -- DVB-Sub font settings must match.
    shadowColor :: Prelude.Maybe DvbSubDestinationShadowColor,
    -- | Specifies the color of the burned-in captions. This option is not valid
    -- for source captions that are STL, 608\/embedded or teletext. These
    -- source settings are already pre-defined by the caption stream. All
    -- burn-in and DVB-Sub font settings must match.
    fontColor :: Prelude.Maybe DvbSubDestinationFontColor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DvbSubDestinationSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundOpacity', 'dvbSubDestinationSettings_backgroundOpacity' - Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'fontOpacity', 'dvbSubDestinationSettings_fontOpacity' - Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
--
-- 'shadowYOffset', 'dvbSubDestinationSettings_shadowYOffset' - Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
--
-- 'fontResolution', 'dvbSubDestinationSettings_fontResolution' - Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'yPosition', 'dvbSubDestinationSettings_yPosition' - Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
--
-- 'backgroundColor', 'dvbSubDestinationSettings_backgroundColor' - Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
--
-- 'shadowXOffset', 'dvbSubDestinationSettings_shadowXOffset' - Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
--
-- 'fontSize', 'dvbSubDestinationSettings_fontSize' - When set to auto fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
--
-- 'xPosition', 'dvbSubDestinationSettings_xPosition' - Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. This option is not valid for source captions that
-- are STL, 608\/embedded or teletext. These source settings are already
-- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
-- must match.
--
-- 'alignment', 'dvbSubDestinationSettings_alignment' - If no explicit xPosition or yPosition is provided, setting alignment to
-- centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. Selecting \"smart\"
-- justification will left-justify live subtitles and center-justify
-- pre-recorded subtitles. This option is not valid for source captions
-- that are STL or 608\/embedded. These source settings are already
-- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
-- must match.
--
-- 'shadowOpacity', 'dvbSubDestinationSettings_shadowOpacity' - Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
--
-- 'teletextGridControl', 'dvbSubDestinationSettings_teletextGridControl' - Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
--
-- 'outlineColor', 'dvbSubDestinationSettings_outlineColor' - Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'outlineSize', 'dvbSubDestinationSettings_outlineSize' - Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'font', 'dvbSubDestinationSettings_font' - External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
--
-- 'shadowColor', 'dvbSubDestinationSettings_shadowColor' - Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
--
-- 'fontColor', 'dvbSubDestinationSettings_fontColor' - Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
newDvbSubDestinationSettings ::
  DvbSubDestinationSettings
newDvbSubDestinationSettings =
  DvbSubDestinationSettings'
    { backgroundOpacity =
        Prelude.Nothing,
      fontOpacity = Prelude.Nothing,
      shadowYOffset = Prelude.Nothing,
      fontResolution = Prelude.Nothing,
      yPosition = Prelude.Nothing,
      backgroundColor = Prelude.Nothing,
      shadowXOffset = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      xPosition = Prelude.Nothing,
      alignment = Prelude.Nothing,
      shadowOpacity = Prelude.Nothing,
      teletextGridControl = Prelude.Nothing,
      outlineColor = Prelude.Nothing,
      outlineSize = Prelude.Nothing,
      font = Prelude.Nothing,
      shadowColor = Prelude.Nothing,
      fontColor = Prelude.Nothing
    }

-- | Specifies the opacity of the background rectangle. 255 is opaque; 0 is
-- transparent. Leaving this parameter blank is equivalent to setting it to
-- 0 (transparent). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_backgroundOpacity = Lens.lens (\DvbSubDestinationSettings' {backgroundOpacity} -> backgroundOpacity) (\s@DvbSubDestinationSettings' {} a -> s {backgroundOpacity = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the burned-in captions. 255 is opaque; 0 is
-- transparent. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontOpacity = Lens.lens (\DvbSubDestinationSettings' {fontOpacity} -> fontOpacity) (\s@DvbSubDestinationSettings' {} a -> s {fontOpacity = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical offset of the shadow relative to the captions in
-- pixels. A value of -2 would result in a shadow offset 2 pixels above the
-- text. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowYOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowYOffset = Lens.lens (\DvbSubDestinationSettings' {shadowYOffset} -> shadowYOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowYOffset = a} :: DvbSubDestinationSettings)

-- | Font resolution in DPI (dots per inch); default is 96 dpi. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontResolution :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_fontResolution = Lens.lens (\DvbSubDestinationSettings' {fontResolution} -> fontResolution) (\s@DvbSubDestinationSettings' {} a -> s {fontResolution = a} :: DvbSubDestinationSettings)

-- | Specifies the vertical position of the caption relative to the top of
-- the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the top of the output. If no explicit yPosition
-- is provided, the caption will be positioned towards the bottom of the
-- output. This option is not valid for source captions that are STL,
-- 608\/embedded or teletext. These source settings are already pre-defined
-- by the caption stream. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_yPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_yPosition = Lens.lens (\DvbSubDestinationSettings' {yPosition} -> yPosition) (\s@DvbSubDestinationSettings' {} a -> s {yPosition = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
dvbSubDestinationSettings_backgroundColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationBackgroundColor)
dvbSubDestinationSettings_backgroundColor = Lens.lens (\DvbSubDestinationSettings' {backgroundColor} -> backgroundColor) (\s@DvbSubDestinationSettings' {} a -> s {backgroundColor = a} :: DvbSubDestinationSettings)

-- | Specifies the horizontal offset of the shadow relative to the captions
-- in pixels. A value of -2 would result in a shadow offset 2 pixels to the
-- left. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowXOffset :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Int)
dvbSubDestinationSettings_shadowXOffset = Lens.lens (\DvbSubDestinationSettings' {shadowXOffset} -> shadowXOffset) (\s@DvbSubDestinationSettings' {} a -> s {shadowXOffset = a} :: DvbSubDestinationSettings)

-- | When set to auto fontSize will scale depending on the size of the
-- output. Giving a positive integer will specify the exact font size in
-- points. All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Text)
dvbSubDestinationSettings_fontSize = Lens.lens (\DvbSubDestinationSettings' {fontSize} -> fontSize) (\s@DvbSubDestinationSettings' {} a -> s {fontSize = a} :: DvbSubDestinationSettings)

-- | Specifies the horizontal position of the caption relative to the left
-- side of the output in pixels. A value of 10 would result in the captions
-- starting 10 pixels from the left of the output. If no explicit xPosition
-- is provided, the horizontal caption position will be determined by the
-- alignment parameter. This option is not valid for source captions that
-- are STL, 608\/embedded or teletext. These source settings are already
-- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
-- must match.
dvbSubDestinationSettings_xPosition :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_xPosition = Lens.lens (\DvbSubDestinationSettings' {xPosition} -> xPosition) (\s@DvbSubDestinationSettings' {} a -> s {xPosition = a} :: DvbSubDestinationSettings)

-- | If no explicit xPosition or yPosition is provided, setting alignment to
-- centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. Selecting \"smart\"
-- justification will left-justify live subtitles and center-justify
-- pre-recorded subtitles. This option is not valid for source captions
-- that are STL or 608\/embedded. These source settings are already
-- pre-defined by the caption stream. All burn-in and DVB-Sub font settings
-- must match.
dvbSubDestinationSettings_alignment :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationAlignment)
dvbSubDestinationSettings_alignment = Lens.lens (\DvbSubDestinationSettings' {alignment} -> alignment) (\s@DvbSubDestinationSettings' {} a -> s {alignment = a} :: DvbSubDestinationSettings)

-- | Specifies the opacity of the shadow. 255 is opaque; 0 is transparent.
-- Leaving this parameter blank is equivalent to setting it to 0
-- (transparent). All burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowOpacity :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_shadowOpacity = Lens.lens (\DvbSubDestinationSettings' {shadowOpacity} -> shadowOpacity) (\s@DvbSubDestinationSettings' {} a -> s {shadowOpacity = a} :: DvbSubDestinationSettings)

-- | Controls whether a fixed grid size will be used to generate the output
-- subtitles bitmap. Only applicable for Teletext inputs and
-- DVB-Sub\/Burn-in outputs.
dvbSubDestinationSettings_teletextGridControl :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationTeletextGridControl)
dvbSubDestinationSettings_teletextGridControl = Lens.lens (\DvbSubDestinationSettings' {teletextGridControl} -> teletextGridControl) (\s@DvbSubDestinationSettings' {} a -> s {teletextGridControl = a} :: DvbSubDestinationSettings)

-- | Specifies font outline color. This option is not valid for source
-- captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationOutlineColor)
dvbSubDestinationSettings_outlineColor = Lens.lens (\DvbSubDestinationSettings' {outlineColor} -> outlineColor) (\s@DvbSubDestinationSettings' {} a -> s {outlineColor = a} :: DvbSubDestinationSettings)

-- | Specifies font outline size in pixels. This option is not valid for
-- source captions that are either 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_outlineSize :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe Prelude.Natural)
dvbSubDestinationSettings_outlineSize = Lens.lens (\DvbSubDestinationSettings' {outlineSize} -> outlineSize) (\s@DvbSubDestinationSettings' {} a -> s {outlineSize = a} :: DvbSubDestinationSettings)

-- | External font file used for caption burn-in. File extension must be
-- \'ttf\' or \'tte\'. Although the user can select output fonts for many
-- different types of input captions, embedded, STL and teletext sources
-- use a strict grid system. Using external fonts with these caption
-- sources could cause unexpected display of proportional fonts. All
-- burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_font :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe InputLocation)
dvbSubDestinationSettings_font = Lens.lens (\DvbSubDestinationSettings' {font} -> font) (\s@DvbSubDestinationSettings' {} a -> s {font = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the shadow cast by the captions. All burn-in and
-- DVB-Sub font settings must match.
dvbSubDestinationSettings_shadowColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationShadowColor)
dvbSubDestinationSettings_shadowColor = Lens.lens (\DvbSubDestinationSettings' {shadowColor} -> shadowColor) (\s@DvbSubDestinationSettings' {} a -> s {shadowColor = a} :: DvbSubDestinationSettings)

-- | Specifies the color of the burned-in captions. This option is not valid
-- for source captions that are STL, 608\/embedded or teletext. These
-- source settings are already pre-defined by the caption stream. All
-- burn-in and DVB-Sub font settings must match.
dvbSubDestinationSettings_fontColor :: Lens.Lens' DvbSubDestinationSettings (Prelude.Maybe DvbSubDestinationFontColor)
dvbSubDestinationSettings_fontColor = Lens.lens (\DvbSubDestinationSettings' {fontColor} -> fontColor) (\s@DvbSubDestinationSettings' {} a -> s {fontColor = a} :: DvbSubDestinationSettings)

instance Core.FromJSON DvbSubDestinationSettings where
  parseJSON =
    Core.withObject
      "DvbSubDestinationSettings"
      ( \x ->
          DvbSubDestinationSettings'
            Prelude.<$> (x Core..:? "backgroundOpacity")
            Prelude.<*> (x Core..:? "fontOpacity")
            Prelude.<*> (x Core..:? "shadowYOffset")
            Prelude.<*> (x Core..:? "fontResolution")
            Prelude.<*> (x Core..:? "yPosition")
            Prelude.<*> (x Core..:? "backgroundColor")
            Prelude.<*> (x Core..:? "shadowXOffset")
            Prelude.<*> (x Core..:? "fontSize")
            Prelude.<*> (x Core..:? "xPosition")
            Prelude.<*> (x Core..:? "alignment")
            Prelude.<*> (x Core..:? "shadowOpacity")
            Prelude.<*> (x Core..:? "teletextGridControl")
            Prelude.<*> (x Core..:? "outlineColor")
            Prelude.<*> (x Core..:? "outlineSize")
            Prelude.<*> (x Core..:? "font")
            Prelude.<*> (x Core..:? "shadowColor")
            Prelude.<*> (x Core..:? "fontColor")
      )

instance Prelude.Hashable DvbSubDestinationSettings where
  hashWithSalt salt' DvbSubDestinationSettings' {..} =
    salt' `Prelude.hashWithSalt` fontColor
      `Prelude.hashWithSalt` shadowColor
      `Prelude.hashWithSalt` font
      `Prelude.hashWithSalt` outlineSize
      `Prelude.hashWithSalt` outlineColor
      `Prelude.hashWithSalt` teletextGridControl
      `Prelude.hashWithSalt` shadowOpacity
      `Prelude.hashWithSalt` alignment
      `Prelude.hashWithSalt` xPosition
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` shadowXOffset
      `Prelude.hashWithSalt` backgroundColor
      `Prelude.hashWithSalt` yPosition
      `Prelude.hashWithSalt` fontResolution
      `Prelude.hashWithSalt` shadowYOffset
      `Prelude.hashWithSalt` fontOpacity
      `Prelude.hashWithSalt` backgroundOpacity

instance Prelude.NFData DvbSubDestinationSettings where
  rnf DvbSubDestinationSettings' {..} =
    Prelude.rnf backgroundOpacity
      `Prelude.seq` Prelude.rnf fontColor
      `Prelude.seq` Prelude.rnf shadowColor
      `Prelude.seq` Prelude.rnf font
      `Prelude.seq` Prelude.rnf outlineSize
      `Prelude.seq` Prelude.rnf outlineColor
      `Prelude.seq` Prelude.rnf teletextGridControl
      `Prelude.seq` Prelude.rnf shadowOpacity
      `Prelude.seq` Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf xPosition
      `Prelude.seq` Prelude.rnf fontSize
      `Prelude.seq` Prelude.rnf shadowXOffset
      `Prelude.seq` Prelude.rnf backgroundColor
      `Prelude.seq` Prelude.rnf yPosition
      `Prelude.seq` Prelude.rnf fontResolution
      `Prelude.seq` Prelude.rnf shadowYOffset
      `Prelude.seq` Prelude.rnf fontOpacity

instance Core.ToJSON DvbSubDestinationSettings where
  toJSON DvbSubDestinationSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("backgroundOpacity" Core..=)
              Prelude.<$> backgroundOpacity,
            ("fontOpacity" Core..=) Prelude.<$> fontOpacity,
            ("shadowYOffset" Core..=) Prelude.<$> shadowYOffset,
            ("fontResolution" Core..=)
              Prelude.<$> fontResolution,
            ("yPosition" Core..=) Prelude.<$> yPosition,
            ("backgroundColor" Core..=)
              Prelude.<$> backgroundColor,
            ("shadowXOffset" Core..=) Prelude.<$> shadowXOffset,
            ("fontSize" Core..=) Prelude.<$> fontSize,
            ("xPosition" Core..=) Prelude.<$> xPosition,
            ("alignment" Core..=) Prelude.<$> alignment,
            ("shadowOpacity" Core..=) Prelude.<$> shadowOpacity,
            ("teletextGridControl" Core..=)
              Prelude.<$> teletextGridControl,
            ("outlineColor" Core..=) Prelude.<$> outlineColor,
            ("outlineSize" Core..=) Prelude.<$> outlineSize,
            ("font" Core..=) Prelude.<$> font,
            ("shadowColor" Core..=) Prelude.<$> shadowColor,
            ("fontColor" Core..=) Prelude.<$> fontColor
          ]
      )
