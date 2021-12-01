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
-- Module      : Amazonka.Pi.Types.MetricQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.MetricQuery where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pi.Types.DimensionGroup
import qualified Amazonka.Prelude as Prelude

-- | A single query to be processed. You must provide the metric to query. If
-- no other parameters are specified, Performance Insights returns all of
-- the data points for that metric. You can optionally request that the
-- data points be aggregated by dimension group ( @GroupBy@), and return
-- only those data points that match your criteria (@Filter@).
--
-- /See:/ 'newMetricQuery' smart constructor.
data MetricQuery = MetricQuery'
  { -- | A specification for how to aggregate the data points from a query
    -- result. You must specify a valid dimension group. Performance Insights
    -- will return all of the dimensions within that group, unless you provide
    -- the names of specific dimensions within that group. You can also request
    -- that Performance Insights return a limited number of values for a
    -- dimension.
    groupBy :: Prelude.Maybe DimensionGroup,
    -- | One or more filters to apply in the request. Restrictions:
    --
    -- -   Any number of filters by the same dimension, as specified in the
    --     @GroupBy@ parameter.
    --
    -- -   A single filter for any other dimension in this dimension group.
    filter' :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- Create a value of 'MetricQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupBy', 'metricQuery_groupBy' - A specification for how to aggregate the data points from a query
-- result. You must specify a valid dimension group. Performance Insights
-- will return all of the dimensions within that group, unless you provide
-- the names of specific dimensions within that group. You can also request
-- that Performance Insights return a limited number of values for a
-- dimension.
--
-- 'filter'', 'metricQuery_filter' - One or more filters to apply in the request. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ parameter.
--
-- -   A single filter for any other dimension in this dimension group.
--
-- 'metric', 'metricQuery_metric' - The name of a Performance Insights metric to be measured.
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
newMetricQuery ::
  -- | 'metric'
  Prelude.Text ->
  MetricQuery
newMetricQuery pMetric_ =
  MetricQuery'
    { groupBy = Prelude.Nothing,
      filter' = Prelude.Nothing,
      metric = pMetric_
    }

-- | A specification for how to aggregate the data points from a query
-- result. You must specify a valid dimension group. Performance Insights
-- will return all of the dimensions within that group, unless you provide
-- the names of specific dimensions within that group. You can also request
-- that Performance Insights return a limited number of values for a
-- dimension.
metricQuery_groupBy :: Lens.Lens' MetricQuery (Prelude.Maybe DimensionGroup)
metricQuery_groupBy = Lens.lens (\MetricQuery' {groupBy} -> groupBy) (\s@MetricQuery' {} a -> s {groupBy = a} :: MetricQuery)

-- | One or more filters to apply in the request. Restrictions:
--
-- -   Any number of filters by the same dimension, as specified in the
--     @GroupBy@ parameter.
--
-- -   A single filter for any other dimension in this dimension group.
metricQuery_filter :: Lens.Lens' MetricQuery (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
metricQuery_filter = Lens.lens (\MetricQuery' {filter'} -> filter') (\s@MetricQuery' {} a -> s {filter' = a} :: MetricQuery) Prelude.. Lens.mapping Lens.coerced

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
metricQuery_metric :: Lens.Lens' MetricQuery Prelude.Text
metricQuery_metric = Lens.lens (\MetricQuery' {metric} -> metric) (\s@MetricQuery' {} a -> s {metric = a} :: MetricQuery)

instance Prelude.Hashable MetricQuery where
  hashWithSalt salt' MetricQuery' {..} =
    salt' `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` groupBy

instance Prelude.NFData MetricQuery where
  rnf MetricQuery' {..} =
    Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf filter'

instance Core.ToJSON MetricQuery where
  toJSON MetricQuery' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GroupBy" Core..=) Prelude.<$> groupBy,
            ("Filter" Core..=) Prelude.<$> filter',
            Prelude.Just ("Metric" Core..= metric)
          ]
      )
