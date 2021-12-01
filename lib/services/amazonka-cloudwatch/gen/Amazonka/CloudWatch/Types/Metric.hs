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
-- Module      : Amazonka.CloudWatch.Types.Metric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Metric where

import Amazonka.CloudWatch.Types.Dimension
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a specific metric.
--
-- /See:/ 'newMetric' smart constructor.
data Metric = Metric'
  { -- | The name of the metric. This is a required field.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the metric.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The dimensions for the metric.
    dimensions :: Prelude.Maybe [Dimension]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Metric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'metric_metricName' - The name of the metric. This is a required field.
--
-- 'namespace', 'metric_namespace' - The namespace of the metric.
--
-- 'dimensions', 'metric_dimensions' - The dimensions for the metric.
newMetric ::
  Metric
newMetric =
  Metric'
    { metricName = Prelude.Nothing,
      namespace = Prelude.Nothing,
      dimensions = Prelude.Nothing
    }

-- | The name of the metric. This is a required field.
metric_metricName :: Lens.Lens' Metric (Prelude.Maybe Prelude.Text)
metric_metricName = Lens.lens (\Metric' {metricName} -> metricName) (\s@Metric' {} a -> s {metricName = a} :: Metric)

-- | The namespace of the metric.
metric_namespace :: Lens.Lens' Metric (Prelude.Maybe Prelude.Text)
metric_namespace = Lens.lens (\Metric' {namespace} -> namespace) (\s@Metric' {} a -> s {namespace = a} :: Metric)

-- | The dimensions for the metric.
metric_dimensions :: Lens.Lens' Metric (Prelude.Maybe [Dimension])
metric_dimensions = Lens.lens (\Metric' {dimensions} -> dimensions) (\s@Metric' {} a -> s {dimensions = a} :: Metric) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML Metric where
  parseXML x =
    Metric'
      Prelude.<$> (x Core..@? "MetricName")
      Prelude.<*> (x Core..@? "Namespace")
      Prelude.<*> ( x Core..@? "Dimensions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable Metric where
  hashWithSalt salt' Metric' {..} =
    salt' `Prelude.hashWithSalt` dimensions
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData Metric where
  rnf Metric' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToQuery Metric where
  toQuery Metric' {..} =
    Prelude.mconcat
      [ "MetricName" Core.=: metricName,
        "Namespace" Core.=: namespace,
        "Dimensions"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> dimensions)
      ]
