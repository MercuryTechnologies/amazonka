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
-- Module      : Amazonka.CloudWatch.Types.Dimension
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Dimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A dimension is a name\/value pair that is part of the identity of a
-- metric. You can assign up to 10 dimensions to a metric. Because
-- dimensions are part of the unique identifier for a metric, whenever you
-- add a unique name\/value pair to one of your metrics, you are creating a
-- new variation of that metric.
--
-- /See:/ 'newDimension' smart constructor.
data Dimension = Dimension'
  { -- | The name of the dimension. Dimension names must contain only ASCII
    -- characters and must include at least one non-whitespace character.
    name :: Prelude.Text,
    -- | The value of the dimension. Dimension values must contain only ASCII
    -- characters and must include at least one non-whitespace character.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dimension_name' - The name of the dimension. Dimension names must contain only ASCII
-- characters and must include at least one non-whitespace character.
--
-- 'value', 'dimension_value' - The value of the dimension. Dimension values must contain only ASCII
-- characters and must include at least one non-whitespace character.
newDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Dimension
newDimension pName_ pValue_ =
  Dimension' {name = pName_, value = pValue_}

-- | The name of the dimension. Dimension names must contain only ASCII
-- characters and must include at least one non-whitespace character.
dimension_name :: Lens.Lens' Dimension Prelude.Text
dimension_name = Lens.lens (\Dimension' {name} -> name) (\s@Dimension' {} a -> s {name = a} :: Dimension)

-- | The value of the dimension. Dimension values must contain only ASCII
-- characters and must include at least one non-whitespace character.
dimension_value :: Lens.Lens' Dimension Prelude.Text
dimension_value = Lens.lens (\Dimension' {value} -> value) (\s@Dimension' {} a -> s {value = a} :: Dimension)

instance Core.FromXML Dimension where
  parseXML x =
    Dimension'
      Prelude.<$> (x Core..@ "Name") Prelude.<*> (x Core..@ "Value")

instance Prelude.Hashable Dimension where
  hashWithSalt salt' Dimension' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` name

instance Prelude.NFData Dimension where
  rnf Dimension' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Core.ToQuery Dimension where
  toQuery Dimension' {..} =
    Prelude.mconcat
      ["Name" Core.=: name, "Value" Core.=: value]
