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
-- Module      : Amazonka.AutoScaling.Types.Filter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter that is used to return a more specific list of
-- results from a describe operation.
--
-- If you specify multiple filters, the filters are joined with an @AND@,
-- and the request returns only results that match all of the specified
-- filters.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/autoscaling-tagging.html Tagging Auto Scaling groups and instances>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | One or more filter values. Filter values are case-sensitive.
    --
    -- If you specify multiple values for a filter, the values are joined with
    -- an @OR@, and the request returns all results that match any of the
    -- specified values. For example, specify \"tag:environment\" for the
    -- filter name and \"production,development\" for the filter values to find
    -- Auto Scaling groups with the tag \"environment=production\" or
    -- \"environment=development\".
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the filter.
    --
    -- The valid values for @Name@ depend on the API operation that you are
    -- including the filter in, DescribeAutoScalingGroups or DescribeTags.
    --
    -- __DescribeAutoScalingGroups__
    --
    -- Valid values for @Name@ include the following:
    --
    -- -   @tag-key@ - Accepts tag keys. The results will only include
    --     information about the Auto Scaling groups associated with these tag
    --     keys.
    --
    -- -   @tag-value@ - Accepts tag values. The results will only include
    --     information about the Auto Scaling groups associated with these tag
    --     values.
    --
    -- -   @tag:\<key>@ - Accepts the key\/value combination of the tag. Use
    --     the tag key in the filter name and the tag value as the filter
    --     value. The results will only include information about the Auto
    --     Scaling groups associated with the specified key\/value combination.
    --
    -- __DescribeTags__
    --
    -- Valid values for @Name@ include the following:
    --
    -- -   @auto-scaling-group@ - Accepts the names of Auto Scaling groups. The
    --     results will only include information about the tags associated with
    --     these Auto Scaling groups.
    --
    -- -   @key@ - Accepts tag keys. The results will only include information
    --     about the tags associated with these tag keys.
    --
    -- -   @value@ - Accepts tag values. The results will only include
    --     information about the tags associated with these tag values.
    --
    -- -   @propagate-at-launch@ - Accepts a boolean value, which specifies
    --     whether tags propagate to instances at launch. The results will only
    --     include information about the tags associated with the specified
    --     boolean value.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'filter_values' - One or more filter values. Filter values are case-sensitive.
--
-- If you specify multiple values for a filter, the values are joined with
-- an @OR@, and the request returns all results that match any of the
-- specified values. For example, specify \"tag:environment\" for the
-- filter name and \"production,development\" for the filter values to find
-- Auto Scaling groups with the tag \"environment=production\" or
-- \"environment=development\".
--
-- 'name', 'filter_name' - The name of the filter.
--
-- The valid values for @Name@ depend on the API operation that you are
-- including the filter in, DescribeAutoScalingGroups or DescribeTags.
--
-- __DescribeAutoScalingGroups__
--
-- Valid values for @Name@ include the following:
--
-- -   @tag-key@ - Accepts tag keys. The results will only include
--     information about the Auto Scaling groups associated with these tag
--     keys.
--
-- -   @tag-value@ - Accepts tag values. The results will only include
--     information about the Auto Scaling groups associated with these tag
--     values.
--
-- -   @tag:\<key>@ - Accepts the key\/value combination of the tag. Use
--     the tag key in the filter name and the tag value as the filter
--     value. The results will only include information about the Auto
--     Scaling groups associated with the specified key\/value combination.
--
-- __DescribeTags__
--
-- Valid values for @Name@ include the following:
--
-- -   @auto-scaling-group@ - Accepts the names of Auto Scaling groups. The
--     results will only include information about the tags associated with
--     these Auto Scaling groups.
--
-- -   @key@ - Accepts tag keys. The results will only include information
--     about the tags associated with these tag keys.
--
-- -   @value@ - Accepts tag values. The results will only include
--     information about the tags associated with these tag values.
--
-- -   @propagate-at-launch@ - Accepts a boolean value, which specifies
--     whether tags propagate to instances at launch. The results will only
--     include information about the tags associated with the specified
--     boolean value.
newFilter ::
  -- | 'name'
  Prelude.Text ->
  Filter
newFilter pName_ =
  Filter' {values = Prelude.Nothing, name = pName_}

-- | One or more filter values. Filter values are case-sensitive.
--
-- If you specify multiple values for a filter, the values are joined with
-- an @OR@, and the request returns all results that match any of the
-- specified values. For example, specify \"tag:environment\" for the
-- filter name and \"production,development\" for the filter values to find
-- Auto Scaling groups with the tag \"environment=production\" or
-- \"environment=development\".
filter_values :: Lens.Lens' Filter (Prelude.Maybe [Prelude.Text])
filter_values = Lens.lens (\Filter' {values} -> values) (\s@Filter' {} a -> s {values = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

-- | The name of the filter.
--
-- The valid values for @Name@ depend on the API operation that you are
-- including the filter in, DescribeAutoScalingGroups or DescribeTags.
--
-- __DescribeAutoScalingGroups__
--
-- Valid values for @Name@ include the following:
--
-- -   @tag-key@ - Accepts tag keys. The results will only include
--     information about the Auto Scaling groups associated with these tag
--     keys.
--
-- -   @tag-value@ - Accepts tag values. The results will only include
--     information about the Auto Scaling groups associated with these tag
--     values.
--
-- -   @tag:\<key>@ - Accepts the key\/value combination of the tag. Use
--     the tag key in the filter name and the tag value as the filter
--     value. The results will only include information about the Auto
--     Scaling groups associated with the specified key\/value combination.
--
-- __DescribeTags__
--
-- Valid values for @Name@ include the following:
--
-- -   @auto-scaling-group@ - Accepts the names of Auto Scaling groups. The
--     results will only include information about the tags associated with
--     these Auto Scaling groups.
--
-- -   @key@ - Accepts tag keys. The results will only include information
--     about the tags associated with these tag keys.
--
-- -   @value@ - Accepts tag values. The results will only include
--     information about the tags associated with these tag values.
--
-- -   @propagate-at-launch@ - Accepts a boolean value, which specifies
--     whether tags propagate to instances at launch. The results will only
--     include information about the tags associated with the specified
--     boolean value.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

instance Prelude.Hashable Filter where
  hashWithSalt salt' Filter' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf name

instance Core.ToQuery Filter where
  toQuery Filter' {..} =
    Prelude.mconcat
      [ "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> values),
        "Name" Core.=: name
      ]
