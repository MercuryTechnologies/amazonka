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
-- Module      : Amazonka.ElasticTranscoder.Types.InputCaptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.InputCaptions where

import qualified Amazonka.Core as Core
import Amazonka.ElasticTranscoder.Types.CaptionSource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The captions to be created, if any.
--
-- /See:/ 'newInputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { -- | A policy that determines how Elastic Transcoder handles the existence of
    -- multiple captions.
    --
    -- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
    --     sidecar captions into outputs. If captions for a language are
    --     embedded in the input file and also appear in a sidecar file,
    --     Elastic Transcoder uses the sidecar captions and ignores the
    --     embedded captions for that language.
    --
    -- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
    --     sidecar captions into outputs. If captions for a language are
    --     embedded in the input file and also appear in a sidecar file,
    --     Elastic Transcoder uses the embedded captions and ignores the
    --     sidecar captions for that language. If @CaptionSources@ is empty,
    --     Elastic Transcoder omits all sidecar captions from the output files.
    --
    -- -   __Override:__ Elastic Transcoder transcodes only the sidecar
    --     captions that you specify in @CaptionSources@.
    --
    -- @MergePolicy@ cannot be null.
    mergePolicy :: Prelude.Maybe Prelude.Text,
    -- | Source files for the input sidecar captions used during the transcoding
    -- process. To omit all sidecar captions, leave @CaptionSources@ blank.
    captionSources :: Prelude.Maybe [CaptionSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputCaptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mergePolicy', 'inputCaptions_mergePolicy' - A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions.
--
-- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the sidecar captions and ignores the
--     embedded captions for that language.
--
-- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the embedded captions and ignores the
--     sidecar captions for that language. If @CaptionSources@ is empty,
--     Elastic Transcoder omits all sidecar captions from the output files.
--
-- -   __Override:__ Elastic Transcoder transcodes only the sidecar
--     captions that you specify in @CaptionSources@.
--
-- @MergePolicy@ cannot be null.
--
-- 'captionSources', 'inputCaptions_captionSources' - Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
newInputCaptions ::
  InputCaptions
newInputCaptions =
  InputCaptions'
    { mergePolicy = Prelude.Nothing,
      captionSources = Prelude.Nothing
    }

-- | A policy that determines how Elastic Transcoder handles the existence of
-- multiple captions.
--
-- -   __MergeOverride:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the sidecar captions and ignores the
--     embedded captions for that language.
--
-- -   __MergeRetain:__ Elastic Transcoder transcodes both embedded and
--     sidecar captions into outputs. If captions for a language are
--     embedded in the input file and also appear in a sidecar file,
--     Elastic Transcoder uses the embedded captions and ignores the
--     sidecar captions for that language. If @CaptionSources@ is empty,
--     Elastic Transcoder omits all sidecar captions from the output files.
--
-- -   __Override:__ Elastic Transcoder transcodes only the sidecar
--     captions that you specify in @CaptionSources@.
--
-- @MergePolicy@ cannot be null.
inputCaptions_mergePolicy :: Lens.Lens' InputCaptions (Prelude.Maybe Prelude.Text)
inputCaptions_mergePolicy = Lens.lens (\InputCaptions' {mergePolicy} -> mergePolicy) (\s@InputCaptions' {} a -> s {mergePolicy = a} :: InputCaptions)

-- | Source files for the input sidecar captions used during the transcoding
-- process. To omit all sidecar captions, leave @CaptionSources@ blank.
inputCaptions_captionSources :: Lens.Lens' InputCaptions (Prelude.Maybe [CaptionSource])
inputCaptions_captionSources = Lens.lens (\InputCaptions' {captionSources} -> captionSources) (\s@InputCaptions' {} a -> s {captionSources = a} :: InputCaptions) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON InputCaptions where
  parseJSON =
    Core.withObject
      "InputCaptions"
      ( \x ->
          InputCaptions'
            Prelude.<$> (x Core..:? "MergePolicy")
            Prelude.<*> ( x Core..:? "CaptionSources"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable InputCaptions where
  hashWithSalt salt' InputCaptions' {..} =
    salt' `Prelude.hashWithSalt` captionSources
      `Prelude.hashWithSalt` mergePolicy

instance Prelude.NFData InputCaptions where
  rnf InputCaptions' {..} =
    Prelude.rnf mergePolicy
      `Prelude.seq` Prelude.rnf captionSources

instance Core.ToJSON InputCaptions where
  toJSON InputCaptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MergePolicy" Core..=) Prelude.<$> mergePolicy,
            ("CaptionSources" Core..=)
              Prelude.<$> captionSources
          ]
      )
