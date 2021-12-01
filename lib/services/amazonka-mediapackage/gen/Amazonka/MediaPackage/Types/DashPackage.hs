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
-- Module      : Amazonka.MediaPackage.Types.DashPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.DashPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackage.Types.AdTriggersElement
import Amazonka.MediaPackage.Types.AdsOnDeliveryRestrictions
import Amazonka.MediaPackage.Types.DashEncryption
import Amazonka.MediaPackage.Types.ManifestLayout
import Amazonka.MediaPackage.Types.PeriodTriggersElement
import Amazonka.MediaPackage.Types.Profile
import Amazonka.MediaPackage.Types.SegmentTemplateFormat
import Amazonka.MediaPackage.Types.StreamSelection
import Amazonka.MediaPackage.Types.UtcTiming
import qualified Amazonka.Prelude as Prelude

-- | A Dynamic Adaptive Streaming over HTTP (DASH) packaging configuration.
--
-- /See:/ 'newDashPackage' smart constructor.
data DashPackage = DashPackage'
  { adsOnDeliveryRestrictions :: Prelude.Maybe AdsOnDeliveryRestrictions,
    -- | Minimum duration (in seconds) that a player will buffer media before
    -- starting the presentation.
    minBufferTimeSeconds :: Prelude.Maybe Prelude.Int,
    -- | Determines the type of UTCTiming included in the Media Presentation
    -- Description (MPD)
    utcTiming :: Prelude.Maybe UtcTiming,
    -- | Determines the type of SegmentTemplate included in the Media
    -- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
    -- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
    -- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
    -- SegmentTemplate, with $Time$ media URLs. When set to
    -- NUMBER_WITH_DURATION, only a duration is included in each
    -- SegmentTemplate, with $Number$ media URLs.
    segmentTemplateFormat :: Prelude.Maybe SegmentTemplateFormat,
    -- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
    -- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
    profile :: Prelude.Maybe Profile,
    -- | Duration (in seconds) of each segment. Actual segments will be rounded
    -- to the nearest multiple of the source segment duration.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    -- | Specifies the value attribute of the UTCTiming field when utcTiming is
    -- set to HTTP-ISO or HTTP-HEAD
    utcTimingUri :: Prelude.Maybe Prelude.Text,
    streamSelection :: Prelude.Maybe StreamSelection,
    encryption :: Prelude.Maybe DashEncryption,
    -- | Minimum duration (in seconds) between potential changes to the Dynamic
    -- Adaptive Streaming over HTTP (DASH) Media Presentation Description
    -- (MPD).
    minUpdatePeriodSeconds :: Prelude.Maybe Prelude.Int,
    -- | Determines the position of some tags in the Media Presentation
    -- Description (MPD). When set to FULL, elements like SegmentTemplate and
    -- ContentProtection are included in each Representation. When set to
    -- COMPACT, duplicate elements are combined and presented at the
    -- AdaptationSet level.
    manifestLayout :: Prelude.Maybe ManifestLayout,
    -- | Duration (in seconds) to delay live content before presentation.
    suggestedPresentationDelaySeconds :: Prelude.Maybe Prelude.Int,
    -- | Time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int,
    adTriggers :: Prelude.Maybe [AdTriggersElement],
    -- | A list of triggers that controls when the outgoing Dynamic Adaptive
    -- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
    -- partitioned into multiple periods. If empty, the content will not be
    -- partitioned into more than one period. If the list contains \"ADS\", new
    -- periods will be created where the Channel source contains SCTE-35 ad
    -- markers.
    periodTriggers :: Prelude.Maybe [PeriodTriggersElement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsOnDeliveryRestrictions', 'dashPackage_adsOnDeliveryRestrictions' - Undocumented member.
--
-- 'minBufferTimeSeconds', 'dashPackage_minBufferTimeSeconds' - Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
--
-- 'utcTiming', 'dashPackage_utcTiming' - Determines the type of UTCTiming included in the Media Presentation
-- Description (MPD)
--
-- 'segmentTemplateFormat', 'dashPackage_segmentTemplateFormat' - Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
--
-- 'profile', 'dashPackage_profile' - The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
--
-- 'segmentDurationSeconds', 'dashPackage_segmentDurationSeconds' - Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
--
-- 'utcTimingUri', 'dashPackage_utcTimingUri' - Specifies the value attribute of the UTCTiming field when utcTiming is
-- set to HTTP-ISO or HTTP-HEAD
--
-- 'streamSelection', 'dashPackage_streamSelection' - Undocumented member.
--
-- 'encryption', 'dashPackage_encryption' - Undocumented member.
--
-- 'minUpdatePeriodSeconds', 'dashPackage_minUpdatePeriodSeconds' - Minimum duration (in seconds) between potential changes to the Dynamic
-- Adaptive Streaming over HTTP (DASH) Media Presentation Description
-- (MPD).
--
-- 'manifestLayout', 'dashPackage_manifestLayout' - Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
--
-- 'suggestedPresentationDelaySeconds', 'dashPackage_suggestedPresentationDelaySeconds' - Duration (in seconds) to delay live content before presentation.
--
-- 'manifestWindowSeconds', 'dashPackage_manifestWindowSeconds' - Time window (in seconds) contained in each manifest.
--
-- 'adTriggers', 'dashPackage_adTriggers' - Undocumented member.
--
-- 'periodTriggers', 'dashPackage_periodTriggers' - A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Channel source contains SCTE-35 ad
-- markers.
newDashPackage ::
  DashPackage
newDashPackage =
  DashPackage'
    { adsOnDeliveryRestrictions =
        Prelude.Nothing,
      minBufferTimeSeconds = Prelude.Nothing,
      utcTiming = Prelude.Nothing,
      segmentTemplateFormat = Prelude.Nothing,
      profile = Prelude.Nothing,
      segmentDurationSeconds = Prelude.Nothing,
      utcTimingUri = Prelude.Nothing,
      streamSelection = Prelude.Nothing,
      encryption = Prelude.Nothing,
      minUpdatePeriodSeconds = Prelude.Nothing,
      manifestLayout = Prelude.Nothing,
      suggestedPresentationDelaySeconds = Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing,
      adTriggers = Prelude.Nothing,
      periodTriggers = Prelude.Nothing
    }

-- | Undocumented member.
dashPackage_adsOnDeliveryRestrictions :: Lens.Lens' DashPackage (Prelude.Maybe AdsOnDeliveryRestrictions)
dashPackage_adsOnDeliveryRestrictions = Lens.lens (\DashPackage' {adsOnDeliveryRestrictions} -> adsOnDeliveryRestrictions) (\s@DashPackage' {} a -> s {adsOnDeliveryRestrictions = a} :: DashPackage)

-- | Minimum duration (in seconds) that a player will buffer media before
-- starting the presentation.
dashPackage_minBufferTimeSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_minBufferTimeSeconds = Lens.lens (\DashPackage' {minBufferTimeSeconds} -> minBufferTimeSeconds) (\s@DashPackage' {} a -> s {minBufferTimeSeconds = a} :: DashPackage)

-- | Determines the type of UTCTiming included in the Media Presentation
-- Description (MPD)
dashPackage_utcTiming :: Lens.Lens' DashPackage (Prelude.Maybe UtcTiming)
dashPackage_utcTiming = Lens.lens (\DashPackage' {utcTiming} -> utcTiming) (\s@DashPackage' {} a -> s {utcTiming = a} :: DashPackage)

-- | Determines the type of SegmentTemplate included in the Media
-- Presentation Description (MPD). When set to NUMBER_WITH_TIMELINE, a full
-- timeline is presented in each SegmentTemplate, with $Number$ media URLs.
-- When set to TIME_WITH_TIMELINE, a full timeline is presented in each
-- SegmentTemplate, with $Time$ media URLs. When set to
-- NUMBER_WITH_DURATION, only a duration is included in each
-- SegmentTemplate, with $Number$ media URLs.
dashPackage_segmentTemplateFormat :: Lens.Lens' DashPackage (Prelude.Maybe SegmentTemplateFormat)
dashPackage_segmentTemplateFormat = Lens.lens (\DashPackage' {segmentTemplateFormat} -> segmentTemplateFormat) (\s@DashPackage' {} a -> s {segmentTemplateFormat = a} :: DashPackage)

-- | The Dynamic Adaptive Streaming over HTTP (DASH) profile type. When set
-- to \"HBBTV_1_5\", HbbTV 1.5 compliant output is enabled.
dashPackage_profile :: Lens.Lens' DashPackage (Prelude.Maybe Profile)
dashPackage_profile = Lens.lens (\DashPackage' {profile} -> profile) (\s@DashPackage' {} a -> s {profile = a} :: DashPackage)

-- | Duration (in seconds) of each segment. Actual segments will be rounded
-- to the nearest multiple of the source segment duration.
dashPackage_segmentDurationSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_segmentDurationSeconds = Lens.lens (\DashPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@DashPackage' {} a -> s {segmentDurationSeconds = a} :: DashPackage)

-- | Specifies the value attribute of the UTCTiming field when utcTiming is
-- set to HTTP-ISO or HTTP-HEAD
dashPackage_utcTimingUri :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Text)
dashPackage_utcTimingUri = Lens.lens (\DashPackage' {utcTimingUri} -> utcTimingUri) (\s@DashPackage' {} a -> s {utcTimingUri = a} :: DashPackage)

-- | Undocumented member.
dashPackage_streamSelection :: Lens.Lens' DashPackage (Prelude.Maybe StreamSelection)
dashPackage_streamSelection = Lens.lens (\DashPackage' {streamSelection} -> streamSelection) (\s@DashPackage' {} a -> s {streamSelection = a} :: DashPackage)

-- | Undocumented member.
dashPackage_encryption :: Lens.Lens' DashPackage (Prelude.Maybe DashEncryption)
dashPackage_encryption = Lens.lens (\DashPackage' {encryption} -> encryption) (\s@DashPackage' {} a -> s {encryption = a} :: DashPackage)

-- | Minimum duration (in seconds) between potential changes to the Dynamic
-- Adaptive Streaming over HTTP (DASH) Media Presentation Description
-- (MPD).
dashPackage_minUpdatePeriodSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_minUpdatePeriodSeconds = Lens.lens (\DashPackage' {minUpdatePeriodSeconds} -> minUpdatePeriodSeconds) (\s@DashPackage' {} a -> s {minUpdatePeriodSeconds = a} :: DashPackage)

-- | Determines the position of some tags in the Media Presentation
-- Description (MPD). When set to FULL, elements like SegmentTemplate and
-- ContentProtection are included in each Representation. When set to
-- COMPACT, duplicate elements are combined and presented at the
-- AdaptationSet level.
dashPackage_manifestLayout :: Lens.Lens' DashPackage (Prelude.Maybe ManifestLayout)
dashPackage_manifestLayout = Lens.lens (\DashPackage' {manifestLayout} -> manifestLayout) (\s@DashPackage' {} a -> s {manifestLayout = a} :: DashPackage)

-- | Duration (in seconds) to delay live content before presentation.
dashPackage_suggestedPresentationDelaySeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_suggestedPresentationDelaySeconds = Lens.lens (\DashPackage' {suggestedPresentationDelaySeconds} -> suggestedPresentationDelaySeconds) (\s@DashPackage' {} a -> s {suggestedPresentationDelaySeconds = a} :: DashPackage)

-- | Time window (in seconds) contained in each manifest.
dashPackage_manifestWindowSeconds :: Lens.Lens' DashPackage (Prelude.Maybe Prelude.Int)
dashPackage_manifestWindowSeconds = Lens.lens (\DashPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@DashPackage' {} a -> s {manifestWindowSeconds = a} :: DashPackage)

-- | Undocumented member.
dashPackage_adTriggers :: Lens.Lens' DashPackage (Prelude.Maybe [AdTriggersElement])
dashPackage_adTriggers = Lens.lens (\DashPackage' {adTriggers} -> adTriggers) (\s@DashPackage' {} a -> s {adTriggers = a} :: DashPackage) Prelude.. Lens.mapping Lens.coerced

-- | A list of triggers that controls when the outgoing Dynamic Adaptive
-- Streaming over HTTP (DASH) Media Presentation Description (MPD) will be
-- partitioned into multiple periods. If empty, the content will not be
-- partitioned into more than one period. If the list contains \"ADS\", new
-- periods will be created where the Channel source contains SCTE-35 ad
-- markers.
dashPackage_periodTriggers :: Lens.Lens' DashPackage (Prelude.Maybe [PeriodTriggersElement])
dashPackage_periodTriggers = Lens.lens (\DashPackage' {periodTriggers} -> periodTriggers) (\s@DashPackage' {} a -> s {periodTriggers = a} :: DashPackage) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DashPackage where
  parseJSON =
    Core.withObject
      "DashPackage"
      ( \x ->
          DashPackage'
            Prelude.<$> (x Core..:? "adsOnDeliveryRestrictions")
            Prelude.<*> (x Core..:? "minBufferTimeSeconds")
            Prelude.<*> (x Core..:? "utcTiming")
            Prelude.<*> (x Core..:? "segmentTemplateFormat")
            Prelude.<*> (x Core..:? "profile")
            Prelude.<*> (x Core..:? "segmentDurationSeconds")
            Prelude.<*> (x Core..:? "utcTimingUri")
            Prelude.<*> (x Core..:? "streamSelection")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "minUpdatePeriodSeconds")
            Prelude.<*> (x Core..:? "manifestLayout")
            Prelude.<*> (x Core..:? "suggestedPresentationDelaySeconds")
            Prelude.<*> (x Core..:? "manifestWindowSeconds")
            Prelude.<*> (x Core..:? "adTriggers" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "periodTriggers"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DashPackage where
  hashWithSalt salt' DashPackage' {..} =
    salt' `Prelude.hashWithSalt` periodTriggers
      `Prelude.hashWithSalt` adTriggers
      `Prelude.hashWithSalt` manifestWindowSeconds
      `Prelude.hashWithSalt` suggestedPresentationDelaySeconds
      `Prelude.hashWithSalt` manifestLayout
      `Prelude.hashWithSalt` minUpdatePeriodSeconds
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` utcTimingUri
      `Prelude.hashWithSalt` segmentDurationSeconds
      `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` segmentTemplateFormat
      `Prelude.hashWithSalt` utcTiming
      `Prelude.hashWithSalt` minBufferTimeSeconds
      `Prelude.hashWithSalt` adsOnDeliveryRestrictions

instance Prelude.NFData DashPackage where
  rnf DashPackage' {..} =
    Prelude.rnf adsOnDeliveryRestrictions
      `Prelude.seq` Prelude.rnf periodTriggers
      `Prelude.seq` Prelude.rnf adTriggers
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf suggestedPresentationDelaySeconds
      `Prelude.seq` Prelude.rnf manifestLayout
      `Prelude.seq` Prelude.rnf minUpdatePeriodSeconds
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf streamSelection
      `Prelude.seq` Prelude.rnf utcTimingUri
      `Prelude.seq` Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf profile
      `Prelude.seq` Prelude.rnf segmentTemplateFormat
      `Prelude.seq` Prelude.rnf utcTiming
      `Prelude.seq` Prelude.rnf minBufferTimeSeconds

instance Core.ToJSON DashPackage where
  toJSON DashPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("adsOnDeliveryRestrictions" Core..=)
              Prelude.<$> adsOnDeliveryRestrictions,
            ("minBufferTimeSeconds" Core..=)
              Prelude.<$> minBufferTimeSeconds,
            ("utcTiming" Core..=) Prelude.<$> utcTiming,
            ("segmentTemplateFormat" Core..=)
              Prelude.<$> segmentTemplateFormat,
            ("profile" Core..=) Prelude.<$> profile,
            ("segmentDurationSeconds" Core..=)
              Prelude.<$> segmentDurationSeconds,
            ("utcTimingUri" Core..=) Prelude.<$> utcTimingUri,
            ("streamSelection" Core..=)
              Prelude.<$> streamSelection,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("minUpdatePeriodSeconds" Core..=)
              Prelude.<$> minUpdatePeriodSeconds,
            ("manifestLayout" Core..=)
              Prelude.<$> manifestLayout,
            ("suggestedPresentationDelaySeconds" Core..=)
              Prelude.<$> suggestedPresentationDelaySeconds,
            ("manifestWindowSeconds" Core..=)
              Prelude.<$> manifestWindowSeconds,
            ("adTriggers" Core..=) Prelude.<$> adTriggers,
            ("periodTriggers" Core..=)
              Prelude.<$> periodTriggers
          ]
      )
