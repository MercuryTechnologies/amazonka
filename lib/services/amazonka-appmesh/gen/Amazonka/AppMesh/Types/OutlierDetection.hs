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
-- Module      : Amazonka.AppMesh.Types.OutlierDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.OutlierDetection where

import Amazonka.AppMesh.Types.Duration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the outlier detection for a virtual node\'s
-- listener.
--
-- /See:/ 'newOutlierDetection' smart constructor.
data OutlierDetection = OutlierDetection'
  { -- | The base amount of time for which a host is ejected.
    baseEjectionDuration :: Duration,
    -- | The time interval between ejection sweep analysis.
    interval :: Duration,
    -- | Maximum percentage of hosts in load balancing pool for upstream service
    -- that can be ejected. Will eject at least one host regardless of the
    -- value.
    maxEjectionPercent :: Prelude.Natural,
    -- | Number of consecutive @5xx@ errors required for ejection.
    maxServerErrors :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutlierDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseEjectionDuration', 'outlierDetection_baseEjectionDuration' - The base amount of time for which a host is ejected.
--
-- 'interval', 'outlierDetection_interval' - The time interval between ejection sweep analysis.
--
-- 'maxEjectionPercent', 'outlierDetection_maxEjectionPercent' - Maximum percentage of hosts in load balancing pool for upstream service
-- that can be ejected. Will eject at least one host regardless of the
-- value.
--
-- 'maxServerErrors', 'outlierDetection_maxServerErrors' - Number of consecutive @5xx@ errors required for ejection.
newOutlierDetection ::
  -- | 'baseEjectionDuration'
  Duration ->
  -- | 'interval'
  Duration ->
  -- | 'maxEjectionPercent'
  Prelude.Natural ->
  -- | 'maxServerErrors'
  Prelude.Natural ->
  OutlierDetection
newOutlierDetection
  pBaseEjectionDuration_
  pInterval_
  pMaxEjectionPercent_
  pMaxServerErrors_ =
    OutlierDetection'
      { baseEjectionDuration =
          pBaseEjectionDuration_,
        interval = pInterval_,
        maxEjectionPercent = pMaxEjectionPercent_,
        maxServerErrors = pMaxServerErrors_
      }

-- | The base amount of time for which a host is ejected.
outlierDetection_baseEjectionDuration :: Lens.Lens' OutlierDetection Duration
outlierDetection_baseEjectionDuration = Lens.lens (\OutlierDetection' {baseEjectionDuration} -> baseEjectionDuration) (\s@OutlierDetection' {} a -> s {baseEjectionDuration = a} :: OutlierDetection)

-- | The time interval between ejection sweep analysis.
outlierDetection_interval :: Lens.Lens' OutlierDetection Duration
outlierDetection_interval = Lens.lens (\OutlierDetection' {interval} -> interval) (\s@OutlierDetection' {} a -> s {interval = a} :: OutlierDetection)

-- | Maximum percentage of hosts in load balancing pool for upstream service
-- that can be ejected. Will eject at least one host regardless of the
-- value.
outlierDetection_maxEjectionPercent :: Lens.Lens' OutlierDetection Prelude.Natural
outlierDetection_maxEjectionPercent = Lens.lens (\OutlierDetection' {maxEjectionPercent} -> maxEjectionPercent) (\s@OutlierDetection' {} a -> s {maxEjectionPercent = a} :: OutlierDetection)

-- | Number of consecutive @5xx@ errors required for ejection.
outlierDetection_maxServerErrors :: Lens.Lens' OutlierDetection Prelude.Natural
outlierDetection_maxServerErrors = Lens.lens (\OutlierDetection' {maxServerErrors} -> maxServerErrors) (\s@OutlierDetection' {} a -> s {maxServerErrors = a} :: OutlierDetection)

instance Core.FromJSON OutlierDetection where
  parseJSON =
    Core.withObject
      "OutlierDetection"
      ( \x ->
          OutlierDetection'
            Prelude.<$> (x Core..: "baseEjectionDuration")
            Prelude.<*> (x Core..: "interval")
            Prelude.<*> (x Core..: "maxEjectionPercent")
            Prelude.<*> (x Core..: "maxServerErrors")
      )

instance Prelude.Hashable OutlierDetection where
  hashWithSalt salt' OutlierDetection' {..} =
    salt' `Prelude.hashWithSalt` maxServerErrors
      `Prelude.hashWithSalt` maxEjectionPercent
      `Prelude.hashWithSalt` interval
      `Prelude.hashWithSalt` baseEjectionDuration

instance Prelude.NFData OutlierDetection where
  rnf OutlierDetection' {..} =
    Prelude.rnf baseEjectionDuration
      `Prelude.seq` Prelude.rnf maxServerErrors
      `Prelude.seq` Prelude.rnf maxEjectionPercent
      `Prelude.seq` Prelude.rnf interval

instance Core.ToJSON OutlierDetection where
  toJSON OutlierDetection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "baseEjectionDuration"
                  Core..= baseEjectionDuration
              ),
            Prelude.Just ("interval" Core..= interval),
            Prelude.Just
              ("maxEjectionPercent" Core..= maxEjectionPercent),
            Prelude.Just
              ("maxServerErrors" Core..= maxServerErrors)
          ]
      )
