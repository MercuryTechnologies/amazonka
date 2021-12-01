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
-- Module      : Amazonka.Glue.GetBlueprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a blueprint.
module Amazonka.Glue.GetBlueprint
  ( -- * Creating a Request
    GetBlueprint (..),
    newGetBlueprint,

    -- * Request Lenses
    getBlueprint_includeParameterSpec,
    getBlueprint_includeBlueprint,
    getBlueprint_name,

    -- * Destructuring the Response
    GetBlueprintResponse (..),
    newGetBlueprintResponse,

    -- * Response Lenses
    getBlueprintResponse_blueprint,
    getBlueprintResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBlueprint' smart constructor.
data GetBlueprint = GetBlueprint'
  { -- | Specifies whether or not to include the parameter specification.
    includeParameterSpec :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether or not to include the blueprint in the response.
    includeBlueprint :: Prelude.Maybe Prelude.Bool,
    -- | The name of the blueprint.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeParameterSpec', 'getBlueprint_includeParameterSpec' - Specifies whether or not to include the parameter specification.
--
-- 'includeBlueprint', 'getBlueprint_includeBlueprint' - Specifies whether or not to include the blueprint in the response.
--
-- 'name', 'getBlueprint_name' - The name of the blueprint.
newGetBlueprint ::
  -- | 'name'
  Prelude.Text ->
  GetBlueprint
newGetBlueprint pName_ =
  GetBlueprint'
    { includeParameterSpec =
        Prelude.Nothing,
      includeBlueprint = Prelude.Nothing,
      name = pName_
    }

-- | Specifies whether or not to include the parameter specification.
getBlueprint_includeParameterSpec :: Lens.Lens' GetBlueprint (Prelude.Maybe Prelude.Bool)
getBlueprint_includeParameterSpec = Lens.lens (\GetBlueprint' {includeParameterSpec} -> includeParameterSpec) (\s@GetBlueprint' {} a -> s {includeParameterSpec = a} :: GetBlueprint)

-- | Specifies whether or not to include the blueprint in the response.
getBlueprint_includeBlueprint :: Lens.Lens' GetBlueprint (Prelude.Maybe Prelude.Bool)
getBlueprint_includeBlueprint = Lens.lens (\GetBlueprint' {includeBlueprint} -> includeBlueprint) (\s@GetBlueprint' {} a -> s {includeBlueprint = a} :: GetBlueprint)

-- | The name of the blueprint.
getBlueprint_name :: Lens.Lens' GetBlueprint Prelude.Text
getBlueprint_name = Lens.lens (\GetBlueprint' {name} -> name) (\s@GetBlueprint' {} a -> s {name = a} :: GetBlueprint)

instance Core.AWSRequest GetBlueprint where
  type AWSResponse GetBlueprint = GetBlueprintResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintResponse'
            Prelude.<$> (x Core..?> "Blueprint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBlueprint where
  hashWithSalt salt' GetBlueprint' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` includeBlueprint
      `Prelude.hashWithSalt` includeParameterSpec

instance Prelude.NFData GetBlueprint where
  rnf GetBlueprint' {..} =
    Prelude.rnf includeParameterSpec
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf includeBlueprint

instance Core.ToHeaders GetBlueprint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetBlueprint" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBlueprint where
  toJSON GetBlueprint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IncludeParameterSpec" Core..=)
              Prelude.<$> includeParameterSpec,
            ("IncludeBlueprint" Core..=)
              Prelude.<$> includeBlueprint,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetBlueprint where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBlueprint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlueprintResponse' smart constructor.
data GetBlueprintResponse = GetBlueprintResponse'
  { -- | Returns a @Blueprint@ object.
    blueprint :: Prelude.Maybe Blueprint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprint', 'getBlueprintResponse_blueprint' - Returns a @Blueprint@ object.
--
-- 'httpStatus', 'getBlueprintResponse_httpStatus' - The response's http status code.
newGetBlueprintResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBlueprintResponse
newGetBlueprintResponse pHttpStatus_ =
  GetBlueprintResponse'
    { blueprint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @Blueprint@ object.
getBlueprintResponse_blueprint :: Lens.Lens' GetBlueprintResponse (Prelude.Maybe Blueprint)
getBlueprintResponse_blueprint = Lens.lens (\GetBlueprintResponse' {blueprint} -> blueprint) (\s@GetBlueprintResponse' {} a -> s {blueprint = a} :: GetBlueprintResponse)

-- | The response's http status code.
getBlueprintResponse_httpStatus :: Lens.Lens' GetBlueprintResponse Prelude.Int
getBlueprintResponse_httpStatus = Lens.lens (\GetBlueprintResponse' {httpStatus} -> httpStatus) (\s@GetBlueprintResponse' {} a -> s {httpStatus = a} :: GetBlueprintResponse)

instance Prelude.NFData GetBlueprintResponse where
  rnf GetBlueprintResponse' {..} =
    Prelude.rnf blueprint
      `Prelude.seq` Prelude.rnf httpStatus
