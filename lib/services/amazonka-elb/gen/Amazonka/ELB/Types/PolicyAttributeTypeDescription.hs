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
-- Module      : Amazonka.ELB.Types.PolicyAttributeTypeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.PolicyAttributeTypeDescription where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy attribute type.
--
-- /See:/ 'newPolicyAttributeTypeDescription' smart constructor.
data PolicyAttributeTypeDescription = PolicyAttributeTypeDescription'
  { -- | The type of the attribute. For example, @Boolean@ or @Integer@.
    attributeType :: Prelude.Maybe Prelude.Text,
    -- | The cardinality of the attribute.
    --
    -- Valid values:
    --
    -- -   ONE(1) : Single value required
    --
    -- -   ZERO_OR_ONE(0..1) : Up to one value is allowed
    --
    -- -   ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
    --
    -- -   ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
    cardinality :: Prelude.Maybe Prelude.Text,
    -- | The default value of the attribute, if applicable.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | A description of the attribute.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyAttributeTypeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeType', 'policyAttributeTypeDescription_attributeType' - The type of the attribute. For example, @Boolean@ or @Integer@.
--
-- 'cardinality', 'policyAttributeTypeDescription_cardinality' - The cardinality of the attribute.
--
-- Valid values:
--
-- -   ONE(1) : Single value required
--
-- -   ZERO_OR_ONE(0..1) : Up to one value is allowed
--
-- -   ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
--
-- -   ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
--
-- 'defaultValue', 'policyAttributeTypeDescription_defaultValue' - The default value of the attribute, if applicable.
--
-- 'attributeName', 'policyAttributeTypeDescription_attributeName' - The name of the attribute.
--
-- 'description', 'policyAttributeTypeDescription_description' - A description of the attribute.
newPolicyAttributeTypeDescription ::
  PolicyAttributeTypeDescription
newPolicyAttributeTypeDescription =
  PolicyAttributeTypeDescription'
    { attributeType =
        Prelude.Nothing,
      cardinality = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      attributeName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The type of the attribute. For example, @Boolean@ or @Integer@.
policyAttributeTypeDescription_attributeType :: Lens.Lens' PolicyAttributeTypeDescription (Prelude.Maybe Prelude.Text)
policyAttributeTypeDescription_attributeType = Lens.lens (\PolicyAttributeTypeDescription' {attributeType} -> attributeType) (\s@PolicyAttributeTypeDescription' {} a -> s {attributeType = a} :: PolicyAttributeTypeDescription)

-- | The cardinality of the attribute.
--
-- Valid values:
--
-- -   ONE(1) : Single value required
--
-- -   ZERO_OR_ONE(0..1) : Up to one value is allowed
--
-- -   ZERO_OR_MORE(0..*) : Optional. Multiple values are allowed
--
-- -   ONE_OR_MORE(1..*0) : Required. Multiple values are allowed
policyAttributeTypeDescription_cardinality :: Lens.Lens' PolicyAttributeTypeDescription (Prelude.Maybe Prelude.Text)
policyAttributeTypeDescription_cardinality = Lens.lens (\PolicyAttributeTypeDescription' {cardinality} -> cardinality) (\s@PolicyAttributeTypeDescription' {} a -> s {cardinality = a} :: PolicyAttributeTypeDescription)

-- | The default value of the attribute, if applicable.
policyAttributeTypeDescription_defaultValue :: Lens.Lens' PolicyAttributeTypeDescription (Prelude.Maybe Prelude.Text)
policyAttributeTypeDescription_defaultValue = Lens.lens (\PolicyAttributeTypeDescription' {defaultValue} -> defaultValue) (\s@PolicyAttributeTypeDescription' {} a -> s {defaultValue = a} :: PolicyAttributeTypeDescription)

-- | The name of the attribute.
policyAttributeTypeDescription_attributeName :: Lens.Lens' PolicyAttributeTypeDescription (Prelude.Maybe Prelude.Text)
policyAttributeTypeDescription_attributeName = Lens.lens (\PolicyAttributeTypeDescription' {attributeName} -> attributeName) (\s@PolicyAttributeTypeDescription' {} a -> s {attributeName = a} :: PolicyAttributeTypeDescription)

-- | A description of the attribute.
policyAttributeTypeDescription_description :: Lens.Lens' PolicyAttributeTypeDescription (Prelude.Maybe Prelude.Text)
policyAttributeTypeDescription_description = Lens.lens (\PolicyAttributeTypeDescription' {description} -> description) (\s@PolicyAttributeTypeDescription' {} a -> s {description = a} :: PolicyAttributeTypeDescription)

instance Core.FromXML PolicyAttributeTypeDescription where
  parseXML x =
    PolicyAttributeTypeDescription'
      Prelude.<$> (x Core..@? "AttributeType")
      Prelude.<*> (x Core..@? "Cardinality")
      Prelude.<*> (x Core..@? "DefaultValue")
      Prelude.<*> (x Core..@? "AttributeName")
      Prelude.<*> (x Core..@? "Description")

instance
  Prelude.Hashable
    PolicyAttributeTypeDescription
  where
  hashWithSalt
    salt'
    PolicyAttributeTypeDescription' {..} =
      salt' `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` attributeName
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` cardinality
        `Prelude.hashWithSalt` attributeType

instance
  Prelude.NFData
    PolicyAttributeTypeDescription
  where
  rnf PolicyAttributeTypeDescription' {..} =
    Prelude.rnf attributeType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf cardinality
