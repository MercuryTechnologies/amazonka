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
-- Module      : Amazonka.OpenSearch.Types.DescribePackagesFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DescribePackagesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.DescribePackagesFilterName
import qualified Amazonka.Prelude as Prelude

-- | A filter to apply to the @DescribePackage@ response.
--
-- /See:/ 'newDescribePackagesFilter' smart constructor.
data DescribePackagesFilter = DescribePackagesFilter'
  { -- | A list of values for the specified field.
    value :: Prelude.Maybe [Prelude.Text],
    -- | Any field from @PackageDetails@.
    name :: Prelude.Maybe DescribePackagesFilterName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePackagesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'describePackagesFilter_value' - A list of values for the specified field.
--
-- 'name', 'describePackagesFilter_name' - Any field from @PackageDetails@.
newDescribePackagesFilter ::
  DescribePackagesFilter
newDescribePackagesFilter =
  DescribePackagesFilter'
    { value = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A list of values for the specified field.
describePackagesFilter_value :: Lens.Lens' DescribePackagesFilter (Prelude.Maybe [Prelude.Text])
describePackagesFilter_value = Lens.lens (\DescribePackagesFilter' {value} -> value) (\s@DescribePackagesFilter' {} a -> s {value = a} :: DescribePackagesFilter) Prelude.. Lens.mapping Lens.coerced

-- | Any field from @PackageDetails@.
describePackagesFilter_name :: Lens.Lens' DescribePackagesFilter (Prelude.Maybe DescribePackagesFilterName)
describePackagesFilter_name = Lens.lens (\DescribePackagesFilter' {name} -> name) (\s@DescribePackagesFilter' {} a -> s {name = a} :: DescribePackagesFilter)

instance Prelude.Hashable DescribePackagesFilter where
  hashWithSalt salt' DescribePackagesFilter' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData DescribePackagesFilter where
  rnf DescribePackagesFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf name

instance Core.ToJSON DescribePackagesFilter where
  toJSON DescribePackagesFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
