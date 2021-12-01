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
-- Module      : Amazonka.MediaConvert.Types.CmafImageBasedTrickPlaySettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafImageBasedTrickPlaySettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.CmafIntervalCadence
import qualified Amazonka.Prelude as Prelude

-- | Tile and thumbnail settings applicable when imageBasedTrickPlay is
-- ADVANCED
--
-- /See:/ 'newCmafImageBasedTrickPlaySettings' smart constructor.
data CmafImageBasedTrickPlaySettings = CmafImageBasedTrickPlaySettings'
  { -- | Number of thumbnails in each row of a tile image. Set a value between 1
    -- and 512.
    tileWidth :: Prelude.Maybe Prelude.Natural,
    -- | Height of each thumbnail within each tile image, in pixels. Leave blank
    -- to maintain aspect ratio with thumbnail width. If following the aspect
    -- ratio would lead to a total tile height greater than 4096, then the job
    -- will be rejected. Must be divisible by 2.
    thumbnailHeight :: Prelude.Maybe Prelude.Natural,
    -- | The cadence MediaConvert follows for generating thumbnails. If set to
    -- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
    -- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
    -- MediaConvert generates thumbnails according to the interval you specify
    -- in thumbnailInterval.
    intervalCadence :: Prelude.Maybe CmafIntervalCadence,
    -- | Width of each thumbnail within each tile image, in pixels. Default is
    -- 312. Must be divisible by 8.
    thumbnailWidth :: Prelude.Maybe Prelude.Natural,
    -- | Enter the interval, in seconds, that MediaConvert uses to generate
    -- thumbnails. If the interval you enter doesn\'t align with the output
    -- frame rate, MediaConvert automatically rounds the interval to align with
    -- the output frame rate. For example, if the output frame rate is 29.97
    -- frames per second and you enter 5, MediaConvert uses a 150 frame
    -- interval to generate thumbnails.
    thumbnailInterval :: Prelude.Maybe Prelude.Double,
    -- | Number of thumbnails in each column of a tile image. Set a value between
    -- 2 and 2048. Must be divisible by 2.
    tileHeight :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CmafImageBasedTrickPlaySettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tileWidth', 'cmafImageBasedTrickPlaySettings_tileWidth' - Number of thumbnails in each row of a tile image. Set a value between 1
-- and 512.
--
-- 'thumbnailHeight', 'cmafImageBasedTrickPlaySettings_thumbnailHeight' - Height of each thumbnail within each tile image, in pixels. Leave blank
-- to maintain aspect ratio with thumbnail width. If following the aspect
-- ratio would lead to a total tile height greater than 4096, then the job
-- will be rejected. Must be divisible by 2.
--
-- 'intervalCadence', 'cmafImageBasedTrickPlaySettings_intervalCadence' - The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
--
-- 'thumbnailWidth', 'cmafImageBasedTrickPlaySettings_thumbnailWidth' - Width of each thumbnail within each tile image, in pixels. Default is
-- 312. Must be divisible by 8.
--
-- 'thumbnailInterval', 'cmafImageBasedTrickPlaySettings_thumbnailInterval' - Enter the interval, in seconds, that MediaConvert uses to generate
-- thumbnails. If the interval you enter doesn\'t align with the output
-- frame rate, MediaConvert automatically rounds the interval to align with
-- the output frame rate. For example, if the output frame rate is 29.97
-- frames per second and you enter 5, MediaConvert uses a 150 frame
-- interval to generate thumbnails.
--
-- 'tileHeight', 'cmafImageBasedTrickPlaySettings_tileHeight' - Number of thumbnails in each column of a tile image. Set a value between
-- 2 and 2048. Must be divisible by 2.
newCmafImageBasedTrickPlaySettings ::
  CmafImageBasedTrickPlaySettings
newCmafImageBasedTrickPlaySettings =
  CmafImageBasedTrickPlaySettings'
    { tileWidth =
        Prelude.Nothing,
      thumbnailHeight = Prelude.Nothing,
      intervalCadence = Prelude.Nothing,
      thumbnailWidth = Prelude.Nothing,
      thumbnailInterval = Prelude.Nothing,
      tileHeight = Prelude.Nothing
    }

-- | Number of thumbnails in each row of a tile image. Set a value between 1
-- and 512.
cmafImageBasedTrickPlaySettings_tileWidth :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
cmafImageBasedTrickPlaySettings_tileWidth = Lens.lens (\CmafImageBasedTrickPlaySettings' {tileWidth} -> tileWidth) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {tileWidth = a} :: CmafImageBasedTrickPlaySettings)

-- | Height of each thumbnail within each tile image, in pixels. Leave blank
-- to maintain aspect ratio with thumbnail width. If following the aspect
-- ratio would lead to a total tile height greater than 4096, then the job
-- will be rejected. Must be divisible by 2.
cmafImageBasedTrickPlaySettings_thumbnailHeight :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
cmafImageBasedTrickPlaySettings_thumbnailHeight = Lens.lens (\CmafImageBasedTrickPlaySettings' {thumbnailHeight} -> thumbnailHeight) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {thumbnailHeight = a} :: CmafImageBasedTrickPlaySettings)

-- | The cadence MediaConvert follows for generating thumbnails. If set to
-- FOLLOW_IFRAME, MediaConvert generates thumbnails for each IDR frame in
-- the output (matching the GOP cadence). If set to FOLLOW_CUSTOM,
-- MediaConvert generates thumbnails according to the interval you specify
-- in thumbnailInterval.
cmafImageBasedTrickPlaySettings_intervalCadence :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe CmafIntervalCadence)
cmafImageBasedTrickPlaySettings_intervalCadence = Lens.lens (\CmafImageBasedTrickPlaySettings' {intervalCadence} -> intervalCadence) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {intervalCadence = a} :: CmafImageBasedTrickPlaySettings)

-- | Width of each thumbnail within each tile image, in pixels. Default is
-- 312. Must be divisible by 8.
cmafImageBasedTrickPlaySettings_thumbnailWidth :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
cmafImageBasedTrickPlaySettings_thumbnailWidth = Lens.lens (\CmafImageBasedTrickPlaySettings' {thumbnailWidth} -> thumbnailWidth) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {thumbnailWidth = a} :: CmafImageBasedTrickPlaySettings)

-- | Enter the interval, in seconds, that MediaConvert uses to generate
-- thumbnails. If the interval you enter doesn\'t align with the output
-- frame rate, MediaConvert automatically rounds the interval to align with
-- the output frame rate. For example, if the output frame rate is 29.97
-- frames per second and you enter 5, MediaConvert uses a 150 frame
-- interval to generate thumbnails.
cmafImageBasedTrickPlaySettings_thumbnailInterval :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Double)
cmafImageBasedTrickPlaySettings_thumbnailInterval = Lens.lens (\CmafImageBasedTrickPlaySettings' {thumbnailInterval} -> thumbnailInterval) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {thumbnailInterval = a} :: CmafImageBasedTrickPlaySettings)

-- | Number of thumbnails in each column of a tile image. Set a value between
-- 2 and 2048. Must be divisible by 2.
cmafImageBasedTrickPlaySettings_tileHeight :: Lens.Lens' CmafImageBasedTrickPlaySettings (Prelude.Maybe Prelude.Natural)
cmafImageBasedTrickPlaySettings_tileHeight = Lens.lens (\CmafImageBasedTrickPlaySettings' {tileHeight} -> tileHeight) (\s@CmafImageBasedTrickPlaySettings' {} a -> s {tileHeight = a} :: CmafImageBasedTrickPlaySettings)

instance
  Core.FromJSON
    CmafImageBasedTrickPlaySettings
  where
  parseJSON =
    Core.withObject
      "CmafImageBasedTrickPlaySettings"
      ( \x ->
          CmafImageBasedTrickPlaySettings'
            Prelude.<$> (x Core..:? "tileWidth")
            Prelude.<*> (x Core..:? "thumbnailHeight")
            Prelude.<*> (x Core..:? "intervalCadence")
            Prelude.<*> (x Core..:? "thumbnailWidth")
            Prelude.<*> (x Core..:? "thumbnailInterval")
            Prelude.<*> (x Core..:? "tileHeight")
      )

instance
  Prelude.Hashable
    CmafImageBasedTrickPlaySettings
  where
  hashWithSalt
    salt'
    CmafImageBasedTrickPlaySettings' {..} =
      salt' `Prelude.hashWithSalt` tileHeight
        `Prelude.hashWithSalt` thumbnailInterval
        `Prelude.hashWithSalt` thumbnailWidth
        `Prelude.hashWithSalt` intervalCadence
        `Prelude.hashWithSalt` thumbnailHeight
        `Prelude.hashWithSalt` tileWidth

instance
  Prelude.NFData
    CmafImageBasedTrickPlaySettings
  where
  rnf CmafImageBasedTrickPlaySettings' {..} =
    Prelude.rnf tileWidth
      `Prelude.seq` Prelude.rnf tileHeight
      `Prelude.seq` Prelude.rnf thumbnailInterval
      `Prelude.seq` Prelude.rnf thumbnailWidth
      `Prelude.seq` Prelude.rnf intervalCadence
      `Prelude.seq` Prelude.rnf thumbnailHeight

instance Core.ToJSON CmafImageBasedTrickPlaySettings where
  toJSON CmafImageBasedTrickPlaySettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tileWidth" Core..=) Prelude.<$> tileWidth,
            ("thumbnailHeight" Core..=)
              Prelude.<$> thumbnailHeight,
            ("intervalCadence" Core..=)
              Prelude.<$> intervalCadence,
            ("thumbnailWidth" Core..=)
              Prelude.<$> thumbnailWidth,
            ("thumbnailInterval" Core..=)
              Prelude.<$> thumbnailInterval,
            ("tileHeight" Core..=) Prelude.<$> tileHeight
          ]
      )
