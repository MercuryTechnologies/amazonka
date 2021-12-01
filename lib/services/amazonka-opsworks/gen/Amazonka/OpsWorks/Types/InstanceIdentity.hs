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
-- Module      : Amazonka.OpsWorks.Types.InstanceIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.InstanceIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains a description of an Amazon EC2 instance from the Amazon EC2
-- metadata service. For more information, see
-- <https://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html Instance Metadata and User Data>.
--
-- /See:/ 'newInstanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { -- | A signature that can be used to verify the document\'s accuracy and
    -- authenticity.
    signature :: Prelude.Maybe Prelude.Text,
    -- | A JSON document that contains the metadata.
    document :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signature', 'instanceIdentity_signature' - A signature that can be used to verify the document\'s accuracy and
-- authenticity.
--
-- 'document', 'instanceIdentity_document' - A JSON document that contains the metadata.
newInstanceIdentity ::
  InstanceIdentity
newInstanceIdentity =
  InstanceIdentity'
    { signature = Prelude.Nothing,
      document = Prelude.Nothing
    }

-- | A signature that can be used to verify the document\'s accuracy and
-- authenticity.
instanceIdentity_signature :: Lens.Lens' InstanceIdentity (Prelude.Maybe Prelude.Text)
instanceIdentity_signature = Lens.lens (\InstanceIdentity' {signature} -> signature) (\s@InstanceIdentity' {} a -> s {signature = a} :: InstanceIdentity)

-- | A JSON document that contains the metadata.
instanceIdentity_document :: Lens.Lens' InstanceIdentity (Prelude.Maybe Prelude.Text)
instanceIdentity_document = Lens.lens (\InstanceIdentity' {document} -> document) (\s@InstanceIdentity' {} a -> s {document = a} :: InstanceIdentity)

instance Prelude.Hashable InstanceIdentity where
  hashWithSalt salt' InstanceIdentity' {..} =
    salt' `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` signature

instance Prelude.NFData InstanceIdentity where
  rnf InstanceIdentity' {..} =
    Prelude.rnf signature
      `Prelude.seq` Prelude.rnf document

instance Core.ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Signature" Core..=) Prelude.<$> signature,
            ("Document" Core..=) Prelude.<$> document
          ]
      )
