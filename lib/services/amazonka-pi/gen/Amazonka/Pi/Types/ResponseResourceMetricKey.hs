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
-- Module      : Amazonka.Pi.Types.ResponseResourceMetricKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.ResponseResourceMetricKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object describing a Performance Insights metric and one or more
-- dimensions for that metric.
--
-- /See:/ 'newResponseResourceMetricKey' smart constructor.
data ResponseResourceMetricKey = ResponseResourceMetricKey'
  { -- | The valid dimensions for the metric.
    dimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of a Performance Insights metric to be measured.
    --
    -- Valid values for @Metric@ are:
    --
    -- -   @db.load.avg@ - a scaled representation of the number of active
    --     sessions for the database engine.
    --
    -- -   @db.sampledload.avg@ - the raw number of active sessions for the
    --     database engine.
    --
    -- If the number of active sessions is less than an internal Performance
    -- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
    -- value. If the number of active sessions is greater than the internal
    -- threshold, Performance Insights samples the active sessions, with
    -- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
    -- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
    -- most use cases, you can query @db.load.avg@ only.
    metric :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResponseResourceMetricKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensions', 'responseResourceMetricKey_dimensions' - The valid dimensions for the metric.
--
-- 'metric', 'responseResourceMetricKey_metric' - The name of a Performance Insights metric to be measured.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
newResponseResourceMetricKey ::
  -- | 'metric'
  Prelude.Text ->
  ResponseResourceMetricKey
newResponseResourceMetricKey pMetric_ =
  ResponseResourceMetricKey'
    { dimensions =
        Prelude.Nothing,
      metric = pMetric_
    }

-- | The valid dimensions for the metric.
responseResourceMetricKey_dimensions :: Lens.Lens' ResponseResourceMetricKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
responseResourceMetricKey_dimensions = Lens.lens (\ResponseResourceMetricKey' {dimensions} -> dimensions) (\s@ResponseResourceMetricKey' {} a -> s {dimensions = a} :: ResponseResourceMetricKey) Prelude.. Lens.mapping Lens.coerced

-- | The name of a Performance Insights metric to be measured.
--
-- Valid values for @Metric@ are:
--
-- -   @db.load.avg@ - a scaled representation of the number of active
--     sessions for the database engine.
--
-- -   @db.sampledload.avg@ - the raw number of active sessions for the
--     database engine.
--
-- If the number of active sessions is less than an internal Performance
-- Insights threshold, @db.load.avg@ and @db.sampledload.avg@ are the same
-- value. If the number of active sessions is greater than the internal
-- threshold, Performance Insights samples the active sessions, with
-- @db.load.avg@ showing the scaled values, @db.sampledload.avg@ showing
-- the raw values, and @db.sampledload.avg@ less than @db.load.avg@. For
-- most use cases, you can query @db.load.avg@ only.
responseResourceMetricKey_metric :: Lens.Lens' ResponseResourceMetricKey Prelude.Text
responseResourceMetricKey_metric = Lens.lens (\ResponseResourceMetricKey' {metric} -> metric) (\s@ResponseResourceMetricKey' {} a -> s {metric = a} :: ResponseResourceMetricKey)

instance Core.FromJSON ResponseResourceMetricKey where
  parseJSON =
    Core.withObject
      "ResponseResourceMetricKey"
      ( \x ->
          ResponseResourceMetricKey'
            Prelude.<$> (x Core..:? "Dimensions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "Metric")
      )

instance Prelude.Hashable ResponseResourceMetricKey where
  hashWithSalt salt' ResponseResourceMetricKey' {..} =
    salt' `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` dimensions

instance Prelude.NFData ResponseResourceMetricKey where
  rnf ResponseResourceMetricKey' {..} =
    Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf metric
