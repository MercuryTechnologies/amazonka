{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.UpdateFleetMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data for a fleet metric.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateFleetMetric>
-- action.
module Amazonka.IoT.UpdateFleetMetric
  ( -- * Creating a Request
    UpdateFleetMetric (..),
    newUpdateFleetMetric,

    -- * Request Lenses
    updateFleetMetric_aggregationType,
    updateFleetMetric_period,
    updateFleetMetric_queryVersion,
    updateFleetMetric_aggregationField,
    updateFleetMetric_expectedVersion,
    updateFleetMetric_queryString,
    updateFleetMetric_description,
    updateFleetMetric_unit,
    updateFleetMetric_metricName,
    updateFleetMetric_indexName,

    -- * Destructuring the Response
    UpdateFleetMetricResponse (..),
    newUpdateFleetMetricResponse,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFleetMetric' smart constructor.
data UpdateFleetMetric = UpdateFleetMetric'
  { -- | The type of the aggregation query.
    aggregationType :: Prelude.Maybe AggregationType,
    -- | The time in seconds between fleet metric emissions. Range [60(1 min),
    -- 86400(1 day)] and must be multiple of 60.
    period :: Prelude.Maybe Prelude.Natural,
    -- | The version of the query.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The field to aggregate.
    aggregationField :: Prelude.Maybe Prelude.Text,
    -- | The expected version of the fleet metric record in the registry.
    expectedVersion :: Prelude.Maybe Prelude.Integer,
    -- | The search query string.
    queryString :: Prelude.Maybe Prelude.Text,
    -- | The description of the fleet metric.
    description :: Prelude.Maybe Prelude.Text,
    -- | Used to support unit transformation such as milliseconds to seconds. The
    -- unit must be supported by
    -- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
    unit :: Prelude.Maybe FleetMetricUnit,
    -- | The name of the fleet metric to update.
    metricName :: Prelude.Text,
    -- | The name of the index to search.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationType', 'updateFleetMetric_aggregationType' - The type of the aggregation query.
--
-- 'period', 'updateFleetMetric_period' - The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
--
-- 'queryVersion', 'updateFleetMetric_queryVersion' - The version of the query.
--
-- 'aggregationField', 'updateFleetMetric_aggregationField' - The field to aggregate.
--
-- 'expectedVersion', 'updateFleetMetric_expectedVersion' - The expected version of the fleet metric record in the registry.
--
-- 'queryString', 'updateFleetMetric_queryString' - The search query string.
--
-- 'description', 'updateFleetMetric_description' - The description of the fleet metric.
--
-- 'unit', 'updateFleetMetric_unit' - Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
--
-- 'metricName', 'updateFleetMetric_metricName' - The name of the fleet metric to update.
--
-- 'indexName', 'updateFleetMetric_indexName' - The name of the index to search.
newUpdateFleetMetric ::
  -- | 'metricName'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  UpdateFleetMetric
newUpdateFleetMetric pMetricName_ pIndexName_ =
  UpdateFleetMetric'
    { aggregationType =
        Prelude.Nothing,
      period = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      aggregationField = Prelude.Nothing,
      expectedVersion = Prelude.Nothing,
      queryString = Prelude.Nothing,
      description = Prelude.Nothing,
      unit = Prelude.Nothing,
      metricName = pMetricName_,
      indexName = pIndexName_
    }

-- | The type of the aggregation query.
updateFleetMetric_aggregationType :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe AggregationType)
updateFleetMetric_aggregationType = Lens.lens (\UpdateFleetMetric' {aggregationType} -> aggregationType) (\s@UpdateFleetMetric' {} a -> s {aggregationType = a} :: UpdateFleetMetric)

-- | The time in seconds between fleet metric emissions. Range [60(1 min),
-- 86400(1 day)] and must be multiple of 60.
updateFleetMetric_period :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Natural)
updateFleetMetric_period = Lens.lens (\UpdateFleetMetric' {period} -> period) (\s@UpdateFleetMetric' {} a -> s {period = a} :: UpdateFleetMetric)

-- | The version of the query.
updateFleetMetric_queryVersion :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Text)
updateFleetMetric_queryVersion = Lens.lens (\UpdateFleetMetric' {queryVersion} -> queryVersion) (\s@UpdateFleetMetric' {} a -> s {queryVersion = a} :: UpdateFleetMetric)

-- | The field to aggregate.
updateFleetMetric_aggregationField :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Text)
updateFleetMetric_aggregationField = Lens.lens (\UpdateFleetMetric' {aggregationField} -> aggregationField) (\s@UpdateFleetMetric' {} a -> s {aggregationField = a} :: UpdateFleetMetric)

-- | The expected version of the fleet metric record in the registry.
updateFleetMetric_expectedVersion :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Integer)
updateFleetMetric_expectedVersion = Lens.lens (\UpdateFleetMetric' {expectedVersion} -> expectedVersion) (\s@UpdateFleetMetric' {} a -> s {expectedVersion = a} :: UpdateFleetMetric)

-- | The search query string.
updateFleetMetric_queryString :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Text)
updateFleetMetric_queryString = Lens.lens (\UpdateFleetMetric' {queryString} -> queryString) (\s@UpdateFleetMetric' {} a -> s {queryString = a} :: UpdateFleetMetric)

-- | The description of the fleet metric.
updateFleetMetric_description :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe Prelude.Text)
updateFleetMetric_description = Lens.lens (\UpdateFleetMetric' {description} -> description) (\s@UpdateFleetMetric' {} a -> s {description = a} :: UpdateFleetMetric)

-- | Used to support unit transformation such as milliseconds to seconds. The
-- unit must be supported by
-- <https://docs.aws.amazon.com/https:/docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_MetricDatum.html CW metric>.
updateFleetMetric_unit :: Lens.Lens' UpdateFleetMetric (Prelude.Maybe FleetMetricUnit)
updateFleetMetric_unit = Lens.lens (\UpdateFleetMetric' {unit} -> unit) (\s@UpdateFleetMetric' {} a -> s {unit = a} :: UpdateFleetMetric)

-- | The name of the fleet metric to update.
updateFleetMetric_metricName :: Lens.Lens' UpdateFleetMetric Prelude.Text
updateFleetMetric_metricName = Lens.lens (\UpdateFleetMetric' {metricName} -> metricName) (\s@UpdateFleetMetric' {} a -> s {metricName = a} :: UpdateFleetMetric)

-- | The name of the index to search.
updateFleetMetric_indexName :: Lens.Lens' UpdateFleetMetric Prelude.Text
updateFleetMetric_indexName = Lens.lens (\UpdateFleetMetric' {indexName} -> indexName) (\s@UpdateFleetMetric' {} a -> s {indexName = a} :: UpdateFleetMetric)

instance Core.AWSRequest UpdateFleetMetric where
  type
    AWSResponse UpdateFleetMetric =
      UpdateFleetMetricResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull UpdateFleetMetricResponse'

instance Prelude.Hashable UpdateFleetMetric where
  hashWithSalt salt' UpdateFleetMetric' {..} =
    salt' `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` expectedVersion
      `Prelude.hashWithSalt` aggregationField
      `Prelude.hashWithSalt` queryVersion
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` aggregationType

instance Prelude.NFData UpdateFleetMetric where
  rnf UpdateFleetMetric' {..} =
    Prelude.rnf aggregationType
      `Prelude.seq` Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf expectedVersion
      `Prelude.seq` Prelude.rnf aggregationField
      `Prelude.seq` Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf period

instance Core.ToHeaders UpdateFleetMetric where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateFleetMetric where
  toJSON UpdateFleetMetric' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("aggregationType" Core..=)
              Prelude.<$> aggregationType,
            ("period" Core..=) Prelude.<$> period,
            ("queryVersion" Core..=) Prelude.<$> queryVersion,
            ("aggregationField" Core..=)
              Prelude.<$> aggregationField,
            ("expectedVersion" Core..=)
              Prelude.<$> expectedVersion,
            ("queryString" Core..=) Prelude.<$> queryString,
            ("description" Core..=) Prelude.<$> description,
            ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just ("indexName" Core..= indexName)
          ]
      )

instance Core.ToPath UpdateFleetMetric where
  toPath UpdateFleetMetric' {..} =
    Prelude.mconcat
      ["/fleet-metric/", Core.toBS metricName]

instance Core.ToQuery UpdateFleetMetric where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFleetMetricResponse' smart constructor.
data UpdateFleetMetricResponse = UpdateFleetMetricResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFleetMetricResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateFleetMetricResponse ::
  UpdateFleetMetricResponse
newUpdateFleetMetricResponse =
  UpdateFleetMetricResponse'

instance Prelude.NFData UpdateFleetMetricResponse where
  rnf _ = ()
