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
-- Module      : Amazonka.ElasticTranscoder.Types.Thumbnails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Thumbnails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Thumbnails for videos.
--
-- /See:/ 'newThumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { -- | Specify one of the following values to control scaling of thumbnails:
    --
    -- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
    --     that you specified in thumbnail MaxWidth or MaxHeight settings
    --     without exceeding the other value.
    --
    -- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
    --     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
    --     and matches or exceeds the other value. Elastic Transcoder centers
    --     the image in thumbnails and then crops in the dimension (if any)
    --     that exceeds the maximum value.
    --
    -- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
    --     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
    --     settings. If the relative proportions of the input video and
    --     thumbnails are different, the thumbnails will be distorted.
    --
    -- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
    --     dimension of the input video exceeds the values that you specified
    --     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
    --     Transcoder crops the thumbnails.
    --
    -- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
    --     their dimensions match the values that you specified for at least
    --     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
    --     value. If you specify this option, Elastic Transcoder does not scale
    --     thumbnails up.
    --
    -- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
    --     their dimensions match the values that you specified for at least
    --     one of @MaxWidth@ and @MaxHeight@ without dropping below either
    --     value. If you specify this option, Elastic Transcoder does not scale
    --     thumbnails up.
    sizingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
    --
    -- You specify whether you want Elastic Transcoder to create thumbnails
    -- when you create a job.
    format :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1080 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 3072.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of thumbnails, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
    -- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
    -- use them together.
    --
    -- The width and height of thumbnail files in pixels. Specify a value in
    -- the format @ width @ x @ height @ where both values are even integers.
    -- The values cannot exceed the width and height that you specified in the
    -- @Video:Resolution@ object.
    resolution :: Prelude.Maybe Prelude.Text,
    -- | To better control resolution and aspect ratio of thumbnails, we
    -- recommend that you use the values @MaxWidth@, @MaxHeight@,
    -- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
    -- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
    -- use them together.
    --
    -- The aspect ratio of thumbnails. Valid values include:
    --
    -- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
    --
    -- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
    -- ratio of the video in the output file.
    aspectRatio :: Prelude.Maybe Prelude.Text,
    -- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
    -- bars to the top and bottom and\/or left and right sides of thumbnails to
    -- make the total size of the thumbnails match the values that you
    -- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
    paddingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The approximate number of seconds between thumbnails. Specify an integer
    -- value.
    interval :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
    -- Transcoder uses 1920 (Full HD) as the default value. If you specify a
    -- numeric value, enter an even integer between 32 and 4096.
    maxWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Thumbnails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizingPolicy', 'thumbnails_sizingPolicy' - Specify one of the following values to control scaling of thumbnails:
--
-- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail MaxWidth or MaxHeight settings
--     without exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
--     and matches or exceeds the other value. Elastic Transcoder centers
--     the image in thumbnails and then crops in the dimension (if any)
--     that exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
--     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
--     settings. If the relative proportions of the input video and
--     thumbnails are different, the thumbnails will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
--     dimension of the input video exceeds the values that you specified
--     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
--     Transcoder crops the thumbnails.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- 'format', 'thumbnails_format' - The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
--
-- 'maxHeight', 'thumbnails_maxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
--
-- 'resolution', 'thumbnails_resolution' - To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in
-- the format @ width @ x @ height @ where both values are even integers.
-- The values cannot exceed the width and height that you specified in the
-- @Video:Resolution@ object.
--
-- 'aspectRatio', 'thumbnails_aspectRatio' - To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the video in the output file.
--
-- 'paddingPolicy', 'thumbnails_paddingPolicy' - When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of thumbnails to
-- make the total size of the thumbnails match the values that you
-- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- 'interval', 'thumbnails_interval' - The approximate number of seconds between thumbnails. Specify an integer
-- value.
--
-- 'maxWidth', 'thumbnails_maxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
newThumbnails ::
  Thumbnails
newThumbnails =
  Thumbnails'
    { sizingPolicy = Prelude.Nothing,
      format = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      resolution = Prelude.Nothing,
      aspectRatio = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      interval = Prelude.Nothing,
      maxWidth = Prelude.Nothing
    }

-- | Specify one of the following values to control scaling of thumbnails:
--
-- -   @Fit@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail MaxWidth or MaxHeight settings
--     without exceeding the other value.
--
-- -   @Fill@: Elastic Transcoder scales thumbnails so they match the value
--     that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings
--     and matches or exceeds the other value. Elastic Transcoder centers
--     the image in thumbnails and then crops in the dimension (if any)
--     that exceeds the maximum value.
--
-- -   @Stretch@: Elastic Transcoder stretches thumbnails to match the
--     values that you specified for thumbnail @MaxWidth@ and @MaxHeight@
--     settings. If the relative proportions of the input video and
--     thumbnails are different, the thumbnails will be distorted.
--
-- -   @Keep@: Elastic Transcoder does not scale thumbnails. If either
--     dimension of the input video exceeds the values that you specified
--     for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic
--     Transcoder crops the thumbnails.
--
-- -   @ShrinkToFit@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
--
-- -   @ShrinkToFill@: Elastic Transcoder scales thumbnails down so that
--     their dimensions match the values that you specified for at least
--     one of @MaxWidth@ and @MaxHeight@ without dropping below either
--     value. If you specify this option, Elastic Transcoder does not scale
--     thumbnails up.
thumbnails_sizingPolicy :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_sizingPolicy = Lens.lens (\Thumbnails' {sizingPolicy} -> sizingPolicy) (\s@Thumbnails' {} a -> s {sizingPolicy = a} :: Thumbnails)

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@.
--
-- You specify whether you want Elastic Transcoder to create thumbnails
-- when you create a job.
thumbnails_format :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_format = Lens.lens (\Thumbnails' {format} -> format) (\s@Thumbnails' {} a -> s {format = a} :: Thumbnails)

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1080 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 3072.
thumbnails_maxHeight :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_maxHeight = Lens.lens (\Thumbnails' {maxHeight} -> maxHeight) (\s@Thumbnails' {} a -> s {maxHeight = a} :: Thumbnails)

-- | To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The width and height of thumbnail files in pixels. Specify a value in
-- the format @ width @ x @ height @ where both values are even integers.
-- The values cannot exceed the width and height that you specified in the
-- @Video:Resolution@ object.
thumbnails_resolution :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_resolution = Lens.lens (\Thumbnails' {resolution} -> resolution) (\s@Thumbnails' {} a -> s {resolution = a} :: Thumbnails)

-- | To better control resolution and aspect ratio of thumbnails, we
-- recommend that you use the values @MaxWidth@, @MaxHeight@,
-- @SizingPolicy@, and @PaddingPolicy@ instead of @Resolution@ and
-- @AspectRatio@. The two groups of settings are mutually exclusive. Do not
-- use them together.
--
-- The aspect ratio of thumbnails. Valid values include:
--
-- @auto@, @1:1@, @4:3@, @3:2@, @16:9@
--
-- If you specify @auto@, Elastic Transcoder tries to preserve the aspect
-- ratio of the video in the output file.
thumbnails_aspectRatio :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_aspectRatio = Lens.lens (\Thumbnails' {aspectRatio} -> aspectRatio) (\s@Thumbnails' {} a -> s {aspectRatio = a} :: Thumbnails)

-- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add black
-- bars to the top and bottom and\/or left and right sides of thumbnails to
-- make the total size of the thumbnails match the values that you
-- specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
thumbnails_paddingPolicy :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_paddingPolicy = Lens.lens (\Thumbnails' {paddingPolicy} -> paddingPolicy) (\s@Thumbnails' {} a -> s {paddingPolicy = a} :: Thumbnails)

-- | The approximate number of seconds between thumbnails. Specify an integer
-- value.
thumbnails_interval :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_interval = Lens.lens (\Thumbnails' {interval} -> interval) (\s@Thumbnails' {} a -> s {interval = a} :: Thumbnails)

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic
-- Transcoder uses 1920 (Full HD) as the default value. If you specify a
-- numeric value, enter an even integer between 32 and 4096.
thumbnails_maxWidth :: Lens.Lens' Thumbnails (Prelude.Maybe Prelude.Text)
thumbnails_maxWidth = Lens.lens (\Thumbnails' {maxWidth} -> maxWidth) (\s@Thumbnails' {} a -> s {maxWidth = a} :: Thumbnails)

instance Core.FromJSON Thumbnails where
  parseJSON =
    Core.withObject
      "Thumbnails"
      ( \x ->
          Thumbnails'
            Prelude.<$> (x Core..:? "SizingPolicy")
            Prelude.<*> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "MaxHeight")
            Prelude.<*> (x Core..:? "Resolution")
            Prelude.<*> (x Core..:? "AspectRatio")
            Prelude.<*> (x Core..:? "PaddingPolicy")
            Prelude.<*> (x Core..:? "Interval")
            Prelude.<*> (x Core..:? "MaxWidth")
      )

instance Prelude.Hashable Thumbnails where
  hashWithSalt salt' Thumbnails' {..} =
    salt' `Prelude.hashWithSalt` maxWidth
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` paddingPolicy
      `Prelude.hashWithSalt` aspectRatio
      `Prelude.hashWithSalt` resolution
      `Prelude.hashWithSalt` maxHeight
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` sizingPolicy

instance Prelude.NFData Thumbnails where
  rnf Thumbnails' {..} =
    Prelude.rnf sizingPolicy
      `Prelude.seq` Prelude.rnf maxWidth
      `Prelude.seq` Prelude.rnf interval
      `Prelude.seq` Prelude.rnf paddingPolicy
      `Prelude.seq` Prelude.rnf aspectRatio
      `Prelude.seq` Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf maxHeight
      `Prelude.seq` Prelude.rnf format

instance Core.ToJSON Thumbnails where
  toJSON Thumbnails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SizingPolicy" Core..=) Prelude.<$> sizingPolicy,
            ("Format" Core..=) Prelude.<$> format,
            ("MaxHeight" Core..=) Prelude.<$> maxHeight,
            ("Resolution" Core..=) Prelude.<$> resolution,
            ("AspectRatio" Core..=) Prelude.<$> aspectRatio,
            ("PaddingPolicy" Core..=) Prelude.<$> paddingPolicy,
            ("Interval" Core..=) Prelude.<$> interval,
            ("MaxWidth" Core..=) Prelude.<$> maxWidth
          ]
      )
