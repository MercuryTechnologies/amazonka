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
-- Module      : Amazonka.MediaLive.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.OutputSettings
import qualified Amazonka.Prelude as Prelude

-- | Output settings. There can be multiple outputs within a group.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The names of the CaptionDescriptions used as caption sources for this
    -- output.
    captionDescriptionNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the VideoDescription used as the source for this output.
    videoDescriptionName :: Prelude.Maybe Prelude.Text,
    -- | The name used to identify an output.
    outputName :: Prelude.Maybe Prelude.Text,
    -- | The names of the AudioDescriptions used as audio sources for this
    -- output.
    audioDescriptionNames :: Prelude.Maybe [Prelude.Text],
    -- | Output type-specific settings.
    outputSettings :: OutputSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captionDescriptionNames', 'output_captionDescriptionNames' - The names of the CaptionDescriptions used as caption sources for this
-- output.
--
-- 'videoDescriptionName', 'output_videoDescriptionName' - The name of the VideoDescription used as the source for this output.
--
-- 'outputName', 'output_outputName' - The name used to identify an output.
--
-- 'audioDescriptionNames', 'output_audioDescriptionNames' - The names of the AudioDescriptions used as audio sources for this
-- output.
--
-- 'outputSettings', 'output_outputSettings' - Output type-specific settings.
newOutput ::
  -- | 'outputSettings'
  OutputSettings ->
  Output
newOutput pOutputSettings_ =
  Output'
    { captionDescriptionNames = Prelude.Nothing,
      videoDescriptionName = Prelude.Nothing,
      outputName = Prelude.Nothing,
      audioDescriptionNames = Prelude.Nothing,
      outputSettings = pOutputSettings_
    }

-- | The names of the CaptionDescriptions used as caption sources for this
-- output.
output_captionDescriptionNames :: Lens.Lens' Output (Prelude.Maybe [Prelude.Text])
output_captionDescriptionNames = Lens.lens (\Output' {captionDescriptionNames} -> captionDescriptionNames) (\s@Output' {} a -> s {captionDescriptionNames = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VideoDescription used as the source for this output.
output_videoDescriptionName :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_videoDescriptionName = Lens.lens (\Output' {videoDescriptionName} -> videoDescriptionName) (\s@Output' {} a -> s {videoDescriptionName = a} :: Output)

-- | The name used to identify an output.
output_outputName :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_outputName = Lens.lens (\Output' {outputName} -> outputName) (\s@Output' {} a -> s {outputName = a} :: Output)

-- | The names of the AudioDescriptions used as audio sources for this
-- output.
output_audioDescriptionNames :: Lens.Lens' Output (Prelude.Maybe [Prelude.Text])
output_audioDescriptionNames = Lens.lens (\Output' {audioDescriptionNames} -> audioDescriptionNames) (\s@Output' {} a -> s {audioDescriptionNames = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | Output type-specific settings.
output_outputSettings :: Lens.Lens' Output OutputSettings
output_outputSettings = Lens.lens (\Output' {outputSettings} -> outputSettings) (\s@Output' {} a -> s {outputSettings = a} :: Output)

instance Core.FromJSON Output where
  parseJSON =
    Core.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> ( x Core..:? "captionDescriptionNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "videoDescriptionName")
            Prelude.<*> (x Core..:? "outputName")
            Prelude.<*> ( x Core..:? "audioDescriptionNames"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "outputSettings")
      )

instance Prelude.Hashable Output where
  hashWithSalt salt' Output' {..} =
    salt' `Prelude.hashWithSalt` outputSettings
      `Prelude.hashWithSalt` audioDescriptionNames
      `Prelude.hashWithSalt` outputName
      `Prelude.hashWithSalt` videoDescriptionName
      `Prelude.hashWithSalt` captionDescriptionNames

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf captionDescriptionNames
      `Prelude.seq` Prelude.rnf outputSettings
      `Prelude.seq` Prelude.rnf audioDescriptionNames
      `Prelude.seq` Prelude.rnf outputName
      `Prelude.seq` Prelude.rnf videoDescriptionName

instance Core.ToJSON Output where
  toJSON Output' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("captionDescriptionNames" Core..=)
              Prelude.<$> captionDescriptionNames,
            ("videoDescriptionName" Core..=)
              Prelude.<$> videoDescriptionName,
            ("outputName" Core..=) Prelude.<$> outputName,
            ("audioDescriptionNames" Core..=)
              Prelude.<$> audioDescriptionNames,
            Prelude.Just
              ("outputSettings" Core..= outputSettings)
          ]
      )
