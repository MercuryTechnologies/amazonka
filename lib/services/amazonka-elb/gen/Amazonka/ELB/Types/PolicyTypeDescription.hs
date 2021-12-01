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
-- Module      : Amazonka.ELB.Types.PolicyTypeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.PolicyTypeDescription where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.PolicyAttributeTypeDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a policy type.
--
-- /See:/ 'newPolicyTypeDescription' smart constructor.
data PolicyTypeDescription = PolicyTypeDescription'
  { -- | The name of the policy type.
    policyTypeName :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The description of the policy attributes associated with the policies
    -- defined by Elastic Load Balancing.
    policyAttributeTypeDescriptions :: Prelude.Maybe [PolicyAttributeTypeDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyTypeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyTypeName', 'policyTypeDescription_policyTypeName' - The name of the policy type.
--
-- 'description', 'policyTypeDescription_description' - A description of the policy type.
--
-- 'policyAttributeTypeDescriptions', 'policyTypeDescription_policyAttributeTypeDescriptions' - The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
newPolicyTypeDescription ::
  PolicyTypeDescription
newPolicyTypeDescription =
  PolicyTypeDescription'
    { policyTypeName =
        Prelude.Nothing,
      description = Prelude.Nothing,
      policyAttributeTypeDescriptions = Prelude.Nothing
    }

-- | The name of the policy type.
policyTypeDescription_policyTypeName :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe Prelude.Text)
policyTypeDescription_policyTypeName = Lens.lens (\PolicyTypeDescription' {policyTypeName} -> policyTypeName) (\s@PolicyTypeDescription' {} a -> s {policyTypeName = a} :: PolicyTypeDescription)

-- | A description of the policy type.
policyTypeDescription_description :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe Prelude.Text)
policyTypeDescription_description = Lens.lens (\PolicyTypeDescription' {description} -> description) (\s@PolicyTypeDescription' {} a -> s {description = a} :: PolicyTypeDescription)

-- | The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
policyTypeDescription_policyAttributeTypeDescriptions :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe [PolicyAttributeTypeDescription])
policyTypeDescription_policyAttributeTypeDescriptions = Lens.lens (\PolicyTypeDescription' {policyAttributeTypeDescriptions} -> policyAttributeTypeDescriptions) (\s@PolicyTypeDescription' {} a -> s {policyAttributeTypeDescriptions = a} :: PolicyTypeDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML PolicyTypeDescription where
  parseXML x =
    PolicyTypeDescription'
      Prelude.<$> (x Core..@? "PolicyTypeName")
      Prelude.<*> (x Core..@? "Description")
      Prelude.<*> ( x Core..@? "PolicyAttributeTypeDescriptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable PolicyTypeDescription where
  hashWithSalt salt' PolicyTypeDescription' {..} =
    salt'
      `Prelude.hashWithSalt` policyAttributeTypeDescriptions
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` policyTypeName

instance Prelude.NFData PolicyTypeDescription where
  rnf PolicyTypeDescription' {..} =
    Prelude.rnf policyTypeName
      `Prelude.seq` Prelude.rnf policyAttributeTypeDescriptions
      `Prelude.seq` Prelude.rnf description
